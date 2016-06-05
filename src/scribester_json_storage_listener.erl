-module(scribester_json_storage_listener).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, notify/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

notify(Event, Server) ->
  gen_server:cast(Server, Event).

%% ------------------------------------------------------------------
%% Records
%% ------------------------------------------------------------------

-record(state, {}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
  ok = scribester_message_event:subscribe(
         ?MODULE, {?MODULE, notify, [?SERVER]}),
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({message_event, Room, User, Body, Time}, State) ->
  EpochStart = calendar:datetime_to_gregorian_seconds(
                 {{1970, 1, 1}, {0, 0, 0}}),
  TimeStamp = calendar:datetime_to_gregorian_seconds(Time) - EpochStart,

  {ok, Dir} = application:get_env(scribester_json_storage_logdir),
  Path = iolist_to_binary([Dir, "/", Room]),

  LineTerm = [
    {type, message},
    {timestamp, TimeStamp},
    {sender, User},
    {message, Body}
  ],
  Line = jsx:encode(LineTerm, [escaped_strings]),

  ok = filelib:ensure_dir(Path),
  {ok, File} = file:open(Path, [append, {encoding, utf8}]),
  io:format(File, "~ts~n", [Line]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  scribester_message_event:unsubscribe(?MODULE).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

