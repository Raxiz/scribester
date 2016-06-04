-module(scribester_frontend_index_handler).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, _Opts) ->
  {ok, Req, no_state}.

handle(Req, State) ->
  {ok, Req2} = cowboy_req:reply(200, [
      {<<"content-type">>, <<"text/html">>}
  ], form_log_index(), Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.


%% Internal functions

form_log_index() ->
  Header = <<"<html><head>",
    "<title>Scribester index</title>",
    "<meta charset=\"utf-8\">",
    "</head><body>">>,

  {ok, Rooms} = application:get_env(scribester, scribester_monitored_rooms),

  Body = [form_room_index(Room) || Room <- Rooms],
  Footer = <<"</body></html>">>,
  [Header, Body, Footer].

form_room_index(Room) ->
  case list_logs(Room) of
    [] ->
      <<>>;
    Logs ->
      FileList = [[<<"<li>">>, make_file_link(Room, File)] || File <- Logs],
      [<<"<p><h1>">>, Room, <<"</h1><p><ul>">>, FileList, <<"</ul></p></p">>]
  end.

make_file_link(Room, File) ->
  Link = [<<"/log/">>, urlencode(Room), <<"/">>, urlencode(File)],
  [<<"<a href=\"">>, Link, <<"\">">>, File, <<"</a">>].

urlencode(S) ->
  http_uri:encode(binary_to_list(iolist_to_binary(S))).

list_logs(Room) ->
  {ok, AllLogsDir} = application:get_env(
                       scribester, scribester_text_storage_logdir),
  Dir = iolist_to_binary([AllLogsDir, "/", Room]),
  Files = case file:list_dir(Dir) of
    {ok, L} -> L;
    {error, enoent} -> [];
    {error, E} -> error(E)
  end,
  lists:sort(Files).
