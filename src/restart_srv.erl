-module(restart_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

-export([start_link/5, force_restart/1]).
-export([call/2, cast/2]).

-callback init(Args :: term()) ->
    {ok, State :: term()} |
    {stop, Reason :: term()} | ignore.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      WrappedPid :: pid(), State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(),
                      WrappedPid :: pid(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout | term(),
                      WrappedPid :: pid(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback on_restart(State :: term()) ->
    {ok, NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    WrappedPid :: pid(),
                    State :: term()) ->
    term().

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Records
%% ------------------------------------------------------------------

-type restart_strategy() ::
  {exponential_backoff, Start :: integer(), Exp :: integer(), Cap :: integer()}.

-record(state, {
          user_state        :: term(),
          module            :: module(),
          wrapped_init      :: function(),
          wrapped_pid       :: pid() | undefined,
          wrapped_mon       :: reference() | undefined,
          terminate_func    :: function() | {exit, term()},
          restart_strategy  :: restart_strategy(),
          restart_data      :: term()
         }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name, Module, WrappedInit, Args, Opts) ->
  {RestartSrvOpts, GenSrvOpts} = separate_options(Opts),
  gen_server:start_link(Name, ?MODULE,
    {Module, WrappedInit, RestartSrvOpts, Args}, GenSrvOpts).

force_restart(Pid) ->
  Pid ! '$restart_srv_do_restart'.

call(Pid, Msg) ->
  gen_server:call(Pid, Msg).

cast(Pid, Msg) ->
  gen_server:cast(Pid, Msg).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Module, WrappedInit, Opts, Args}) ->
  case Module:init(Args) of
    {ok, UserState} ->
      self() ! '$restart_srv_init',
      TerminateFunc = proplists:get_value(terminate_func, Opts,
                                      {exit, shutdown}),
      RestartStrategy = proplists:get_value(restart_strategy, Opts,
                                      {exponential_backoff, 1000, 2, 300000}),
      {ok, #state{
        user_state = UserState,
        module = Module,
        wrapped_init = WrappedInit,
        wrapped_pid = undefined,
        terminate_func = TerminateFunc,
        restart_strategy = RestartStrategy,
        restart_data = init_restart_data(RestartStrategy)
      }};
    E ->
      E
  end.

handle_call(Request, From,
            State=#state{user_state=US, module=Mod, wrapped_pid=Pid}) ->
  case Mod:handle_call(Request, From, Pid, US) of
    {reply, Reply, NUS} ->
      {reply, Reply, State#state{user_state=NUS}};
    {reply, Reply, NUS, Timeout} ->
      {reply, Reply, State#state{user_state=NUS}, Timeout};
    {noreply, NUS} ->
      {noreply, State#state{user_state=NUS}};
    {noreply, NUS, Timeout} ->
      {noreply, State#state{user_state=NUS}, Timeout};
    {stop, Reason, Reply, NUS} ->
      {stop, Reason, Reply, State#state{user_state=NUS}};
    {stop, Reason, NUS} ->
      {stop, Reason, State#state{user_state=NUS}}
  end.

handle_cast(Request,
            State=#state{user_state=US, module=Mod, wrapped_pid=Pid}) ->
  case Mod:handle_cast(Request, Pid, US) of
    {noreply, NUS} ->
      {noreply, State#state{user_state=NUS}};
    {noreply, NUS, Timeout} ->
      {noreply, State#state{user_state=NUS}, Timeout};
    {stop, Reason, NUS} ->
      {stop, Reason, State#state{user_state=NUS}}
  end.

handle_info('$restart_srv_init', State) ->

  case try_restart_wrapped_service(State, true) of
    {ok, NewState} ->
      {noreply, NewState};
    {stop, Reason} ->
      {stop, Reason}
  end;
handle_info('$restart_srv_do_restart',
            State=#state{wrapped_pid=undefined}) ->

  case try_restart_wrapped_service(State, false) of
    {ok, NewState} ->
      {noreply, NewState};
    {stop, Reason} ->
      {stop, Reason}
  end;
handle_info('$restart_srv_do_restart',
            State=#state{wrapped_pid=Pid,
                         wrapped_mon=Mon,
                         terminate_func=TermFunc}) when is_pid(Pid) ->
  demonitor(Mon),
  do_terminate_wrapped(Pid, TermFunc),
  case try_restart_wrapped_service(State, false) of
    {ok, NewState} ->
      {noreply, NewState};
    {stop, Reason} ->
      {stop, Reason}
  end;
handle_info({'DOWN', _MonRef, process, Pid, Info},
            State=#state{wrapped_pid=Pid}) ->
  error_logger:error_msg(
    "restart_srv: wrapped service down: ~p~n", [Info]),
  case try_restart_wrapped_service(State, false) of
    {ok, NewState} ->
      {noreply, NewState};
    {stop, Reason} ->
      {stop, Reason}
  end;
handle_info(Msg,
            State=#state{user_state=US, module=Mod, wrapped_pid=Pid}) ->
  case Mod:handle_info(Msg, Pid, US) of
    {noreply, NUS} ->
      {noreply, State#state{user_state=NUS}};
    {noreply, NUS, Timeout} ->
      {noreply, State#state{user_state=NUS}, Timeout};
    {stop, Reason, NUS} ->
      {stop, Reason, State#state{user_state=NUS}}
  end.

terminate(Reason, #state{wrapped_pid=undefined, user_state=US, module=Mod}) ->
  Mod:terminate(Reason, undefined, US);
terminate(Reason, #state{wrapped_pid=Pid, terminate_func=TermFunc,
                          user_state=US, module=Mod}) ->
  Mod:terminate(Reason, Pid, US),
  do_terminate_wrapped(Pid, TermFunc).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

separate_options(Opts) ->
  InternalOpts = [terminate_func, restart_strategy],
  separate_options(Opts, InternalOpts, {[], []}).

separate_options([], _InternalOpts, Result) ->
  Result;
separate_options([O|T], InternalOpts, {Internal, External}) ->
  Key = case O of
    {K, _V} -> K;
    _ -> O
  end,
  case lists:member(Key, InternalOpts) of
    true ->
      separate_options(T, InternalOpts, {[O|Internal], External});
    false ->
      separate_options(T, InternalOpts, {Internal, [O|External]})
  end.

init_restart_data({exponential_backoff, _, _, _}) ->
  0.

reset_restart_data(RestartStrategy, _OldData) ->
  init_restart_data(RestartStrategy).

next_restart({exponential_backoff, First, Exp, Cap}, Next) ->
  case Next of
    0 ->
      {Next, First};
    _ ->
      Next2 = Next*Exp,
      case Next2 > Cap of
        true -> {Next, Cap};
        false -> {Next, Next2}
      end
  end.

init_wrapped_service(WrappedInit) when is_function(WrappedInit) ->
  try WrappedInit() of
    R -> R
  catch
    throw:T ->
      {error, {throw, T}};
    error:E ->
      {error, E};
    exit:E ->
      {error, {exit, E}}
  end.

try_restart_wrapped_service(
            State=#state{
                     module=Module,
                     user_state=US,
                     wrapped_init=WrappedInit,
                     restart_strategy=RestartStrategy,
                     restart_data=RestartData},
            Initial) ->
  case init_wrapped_service(WrappedInit) of
    {ok, NewPid} ->
      Mon = monitor(process, NewPid),
      NS = case Initial of
        true ->
          {ok, US};
        false ->
          Module:on_restart(US)
      end,

      case NS of
        {ok, NUS} ->
          NewRestartData = reset_restart_data(RestartStrategy, RestartData),
          NewState = State#state{
                       user_state=NUS,
                       wrapped_pid=NewPid,
                       wrapped_mon=Mon,
                       restart_data=NewRestartData
                      },
          {ok, NewState};
        {stop, Reason} ->
          {stop, Reason}
      end;
    E ->
      error_logger:error_msg(
        "restart_srv: starting wrapped service failed: ~p~n", [E]),
      case next_restart(RestartStrategy, RestartData) of
        {0, Data} ->
          self() ! '$restart_srv_do_restart',
          {ok, State#state{restart_data=Data}};
        {Timeout, Data} when Timeout > 0 ->
          erlang:send_after(Timeout, self(), '$restart_srv_do_restart'),
          {ok, State#state{restart_data=Data}}
      end
  end.

do_terminate_wrapped(Pid, {exit, Reason}) ->
  exit(Pid, Reason),
  ok;
do_terminate_wrapped(Pid, Func) when is_function(Func) ->
  Func(Pid).
