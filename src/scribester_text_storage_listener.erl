-module(scribester_text_storage_listener).
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
  {ok, TimeZone} = application:get_env(scribester_timezone),
  {ok, TimeFormat} = application:get_env(scribester_timeformat),
  TimeStr = qdate:to_string(TimeFormat, TimeZone, Time),
  Line = ["[", TimeStr, "] ", User, ": ", Body, "\n"],
  NState = append_line_to_room_log(Room, Time, Line, State),
  {noreply, NState}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  scribester_message_event:unsubscribe(?MODULE).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

append_line_to_room_log(Room, Time, Line, State) ->
  {ok, TimeZone} = application:get_env(scribester_timezone),
  Date = qdate:to_string("Y-m-d", TimeZone, Time),
  {ok, AllLogsDir} = application:get_env(scribester_text_storage_logdir),
  Dir = iolist_to_binary([AllLogsDir, "/", Room, "/"]),
  Path = iolist_to_binary([Dir, "/", Date, ".log"]),

  ok = filelib:ensure_dir(Dir),
  {ok, File} = file:open(Path, [append, {encoding, utf8}]),
  io:format(File, "~ts", [Line]),
  file:close(File),
  State.
