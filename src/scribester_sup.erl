-module(scribester_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, ListenerModules} = application:get_env(scribester_listeners),
  ListenerSpecs = [?CHILD(M, worker) || M <- ListenerModules],

  {ok, { {one_for_one, 5, 10}, [
      ?CHILD(scribester_bot, worker),
      ?CHILD(scribester_message_event, worker)
    ] ++ ListenerSpecs
  } }.

