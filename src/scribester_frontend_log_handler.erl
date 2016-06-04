-module(scribester_frontend_log_handler).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, _Opts) ->
  {ok, Req, no_state}.

handle(Req, State) ->
  {Room, _} = cowboy_req:binding(room, Req),
  {File, _} = cowboy_req:binding(file, Req),
  {ok, Req2} = case valid_params(Room, File) of
    true ->
      cowboy_req:reply(200, [
          {<<"content-type">>, <<"text/plain">>}
        ], form_body(Room, File), Req);
    false ->
      cowboy_req:reply(400, [
          {<<"content-type">>, <<"text/plain">>}
        ], <<"Invalid params">>, Req)
  end,
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

%% Internal functions

valid_params(Room, File) ->
  {ok, Rooms} = application:get_env(scribester, scribester_monitored_rooms),
  RoomStr = binary_to_list(iolist_to_binary(Room)),
  case lists:member(RoomStr, Rooms) of
    true ->
      valid_filename(iolist_to_binary(File));
    false ->
      false
  end.

valid_filename(<<>>) ->
  false;
valid_filename(F) ->
  valid_filename_internal(F).

valid_filename_internal(<<>>) ->
  true;
valid_filename_internal(<<H, Rest/binary>>) when
    H >= $a, H =< $z; H >= $A, H =< $Z; H >= $0, H =< $9;
    H == $.; H == $- ->
  valid_filename_internal(Rest);
valid_filename_internal(_) ->
  false.

form_body(Room, File) ->
  {ok, AllLogsDir} = application:get_env(
                       scribester, scribester_text_storage_logdir),
  Path = iolist_to_binary([AllLogsDir, "/", Room, "/", File]),
  {ok, Text} = file:read_file(Path),
  Text.
