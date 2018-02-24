-module(scribester_app).

-define(APP_NAME, scribester).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([start/0]).

start() ->
  ensure_started(exmpp, permanent),
  ensure_started(crypto, permanent),
  ensure_started(asn1, permanent),
  ensure_started(public_key, permanent),
  ensure_started(ssl, permanent),
  ensure_started(ranch, permanent),
  ensure_started(cowlib, permanent),
  ensure_started(cowboy, permanent),
  ensure_started(erlexec, permanent),
  ensure_started(scribester, transient).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  ensure_mandatory_env(),
  case application:get_env(?APP_NAME, scribester_frontend_enable) of
    {ok, true} ->
      start_frontend();
    {ok, false} ->
      ok
  end,
  scribester_sup:start_link().

stop(_State) ->
  ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started(App, S) ->
  case application:start(App, S) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok;
    E ->
      error(E)
  end.

ensure_mandatory_env() ->
  ensure_mandatory_param(scribester_server),
  ensure_mandatory_param(scribester_username),
  ensure_mandatory_param(scribester_password),
  ensure_mandatory_param(scribester_monitored_rooms),

  ensure_mandatory_param(scribester_text_storage_logdir).

ensure_mandatory_param(Param) ->
  {ok, _} = application:get_env(?APP_NAME, Param).

start_frontend() ->
  {ok, StrIP} = application:get_env(?APP_NAME, scribester_frontend_ip),
  {ok, Port} = application:get_env(?APP_NAME, scribester_frontend_port),
  {ok, Options} = application:get_env(?APP_NAME, scribester_frontend_options),

  {ok, IP} = inet_parse:address(StrIP),

  Dispatch = cowboy_router:compile([{'_', [
      {"/", scribester_frontend_index_handler, []},
      {"/log/:room/:file", scribester_frontend_log_handler, []}
  ]}]),
  {ok, _} = cowboy:start_http(scribester_frontend_listener, 100, [
              {ip, IP},
              {port, Port} | Options
          ], [{env, [{dispatch, Dispatch}]}]).


