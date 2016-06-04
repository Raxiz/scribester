-module(scribester_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([start/0]).

start() ->
  ok = application:start(exmpp, permanent),
  ok = application:start(scribester, transient).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  scribester_sup:start_link().

stop(_State) ->
  ok.
