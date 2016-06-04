-module(scribester_app).

-define(APP_NAME, scribester).

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
  ensure_mandatory_env(),
  scribester_sup:start_link().

stop(_State) ->
  ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_mandatory_env() ->
  ensure_mandatory_param(scribester_server),
  ensure_mandatory_param(scribester_username),
  ensure_mandatory_param(scribester_password),
  ensure_mandatory_param(scribester_monitored_rooms),

  ensure_mandatory_param(scribester_text_storage_logdir).

ensure_mandatory_param(Param) ->
  {ok, _} = application:get_env(?APP_NAME, Param).
