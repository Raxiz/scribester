-module(scribester_simple_message_listener).
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
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  ok = scribester_message_event:subscribe(
         ?MODULE, {?MODULE, notify, [?SERVER]}),
  {ok, Args}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({message_event, Room, User, Body, Time}, State) ->
  {ok, TimeZone} = application:get_env(scribester_timezone),
  {ok, TimeFormat} = application:get_env(scribester_timeformat),
  TimeStr = qdate:to_string(TimeFormat, TimeZone, Time),
  io:format("[~s] (~s) ~s: ~s~n", [Room, TimeStr, User, Body]),
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

