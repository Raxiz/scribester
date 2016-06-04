-module(scribester_message_event).
-behaviour(gen_event).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         subscribe/2,
         unsubscribe/1,
         message_event/4]).

%% ------------------------------------------------------------------
%% gen_event Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_event:start_link({local, ?MODULE}).

subscribe(Id, {_M,_F,_A} = Callback) ->
  gen_event:add_sup_handler(?MODULE, {?MODULE, Id}, Callback).

unsubscribe(Id) ->
  gen_event:delete_handler(?MODULE, {?MODULE, Id}, undefined).

message_event(Room, User, Body, Time) ->
  catch gen_event:notify(?MODULE, {message_event, Room, User, Body, Time}).

%% ------------------------------------------------------------------
%% gen_event Function Definitions
%% ------------------------------------------------------------------

init(Callback) ->
  {ok, Callback}.

handle_event(Event, {M, F, A} = Callback) ->
  erlang:apply(M, F, [Event|A]),
  {ok, Callback}.

handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

