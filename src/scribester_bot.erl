-module(scribester_bot).
-behaviour(restart_srv).
-define(SERVER, ?MODULE).

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% restart_srv Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/4, handle_cast/3, handle_info/3,
         on_restart/1, terminate/3]).

%% ------------------------------------------------------------------
%% Records
%% ------------------------------------------------------------------

-record(state, {
    logging_enabled = true,
    log_old_messages = true
  }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  restart_srv:start_link({local, ?SERVER},
                         ?MODULE, fun start_session/0, [],
                         [{terminate_func, fun exmpp_session:stop/1}]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  {ok, #state{}}.

handle_call(_Request, _From, _Pid, State) ->
  {reply, ok, State}.

handle_cast(_Msg, _Pid, State) ->
  {noreply, State}.

handle_info(#received_packet{} = Packet, Pid, State) ->
  %% io:format("~p~n", [Packet]),
  NState = handle_xmpp_packet(Packet, Pid, State),
  {noreply, NState};

handle_info(_Info, _Pid, State) ->
  {noreply, State}.

on_restart(State) ->
  {ok, State#state{log_old_messages=false}}.

terminate(_Reason, _Pid, _State) ->
  ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_session() ->
  {ok, Username} = application:get_env(scribester_username),
  {ok, Server} = application:get_env(scribester_server),
  {ok, Password} = application:get_env(scribester_password),
  {ok, Resource} = application:get_env(scribester_resource),

  {ok, UseSSL} = application:get_env(scribester_use_ssl),
  Port = case application:get_env(scribester_port) of
    {ok, N} when is_integer(N) ->
      N;
    {ok, undefined} ->
      case UseSSL of
        true -> 5223;
        false -> 5222
      end
  end,

  {ok, Rooms} = application:get_env(scribester_monitored_rooms),

  Session = exmpp_session:start(),
  JID = exmpp_jid:make(Username, Server, Resource),
  ok  = exmpp_session:auth_basic_digest(Session, JID, Password),
  case UseSSL of
    true ->
      {ok, _} = exmpp_session:connect_SSL(Session, Server, Port,
                                          [{whitespace_ping, 10}]);
    false ->
      {ok, _} = exmpp_session:connect_TCP(Session, Server, Port,
                                          [{whitespace_ping, 10}])
  end,
  {ok, _} = exmpp_session:login(Session),
  exmpp_session:send_packet(Session,
                    exmpp_presence:set_status(exmpp_presence:available(), "")),
  [join_room(Session, R) || R <- Rooms],
  {ok, Session}.

join_room(Session, Room) ->
  {ok, Username} = application:get_env(scribester_username),
  exmpp_session:send_packet(Session,
    exmpp_stanza:set_recipient(exmpp_presence:available(),
                               Room ++ "/" ++ Username)),
  ok. %TODO: find a way to see if join have failed.

handle_message_in_room(Room, From, Msg, Time, _Pid,
                       #state{logging_enabled=LoggingEnabled}) ->
  case LoggingEnabled of
    true ->
      ok = scribester_message_event:message_event(Room, From, Msg, Time);
    false ->
      ok
  end.

handle_xmpp_packet(#received_packet{
          packet_type=message,
          type_attr="groupchat",
          from={RoomName, RoomServer, From},
          raw_packet=Raw}, Pid,
         State = #state{log_old_messages = LogOld}) when From /= undefined ->
  Room = [RoomName, "@", RoomServer],
  Body = exmpp_message:get_body(Raw),
  case LogOld orelse not is_old_message(Raw) of
    true ->
      Time = extract_timestamp(Raw),
      handle_message_in_room(Room, From, Body, Time, Pid, State),
      case application:get_env(scribester_special_commands_enabled) of
        {ok, true} ->
          case is_old_message(Raw) of
            true ->
              State;
            false ->
              look_for_special_command(Body, Room, Pid, State)
          end;
        {ok, false} ->
          State
      end;
    false ->
      State
  end;
handle_xmpp_packet(Packet = #received_packet{
           packet_type=presence,
           type_attr="unavailable",
           from={_RoomName, _RoomServer, From}}, _Pid,
          State) ->
  {ok, Username} = application:get_env(scribester_username),
  case iolist_to_binary(From) == iolist_to_binary(Username) of
    true ->
      error_logger:error_msg(
        "scribester_bot: received 'unavailable' packet: ~p~n", [Packet]),
      restart_srv:force_restart(self()),
      State;
    false ->
      State
  end;
handle_xmpp_packet(_, _Pid, State) ->
  State.

is_old_message(Raw) ->
  exmpp_xml:get_element(Raw, 'urn:xmpp:delay', delay) /= undefined.

extract_timestamp(Raw) ->
  Delay = exmpp_xml:get_element(Raw, 'urn:xmpp:delay', delay),
  Stamp = exmpp_xml:get_attribute(Delay, <<"stamp">>, undefined),
  case Stamp of
    undefined ->
      calendar:universal_time();
    _ ->
      iso8601:parse(iolist_to_binary(Stamp))
  end.

look_for_special_command(Body, Room, Pid, State) ->
  {ok, Username} = application:get_env(scribester_username),
  Prefix = <<(iolist_to_binary(Username))/binary, " ">>,
  PrefixSize = size(Prefix),
  case Body of
    <<Prefix:PrefixSize/binary, Rest/binary>> ->
      handle_special_command(Rest, Room, Pid, State);
    _ ->
      State
  end.

handle_special_command(<<"off">>, Room, Pid, State) ->
  send_message_to_room(<<"Logging off">>, Room, Pid, State),
  State#state{logging_enabled=false};

handle_special_command(<<"on">>, Room, Pid, State) ->
  send_message_to_room(<<"Logging on">>, Room, Pid, State),
  State#state{logging_enabled=true};

handle_special_command(<<"status">>, Room, Pid, State) ->
  send_message_to_room(status_message(State), Room, Pid, State),
  State;

handle_special_command(Cmd, Room, Pid, State) ->
  case find_external_command(Cmd) of
    {ok, Script} ->
      Params = [{config, export_config()}, {room, iolist_to_binary(Room)}, {command, Cmd}],
      Stdin = jsx:encode(Params),
      case run_script(Script, Stdin) of
        {ok, Output} ->
          send_message_to_room(Output, Room, Pid, State);
        {error, Reason, Output, Stderr} ->
          Message = [<<"Error ">>, io_lib:format("~p", [Reason]), <<".\nStdout:\n">>, Output, <<"\n\nStderr:\n">>, Stderr],
          send_message_to_room(Message, Room, Pid, State)
      end;
    {error, notfound} ->
      send_message_to_room(<<"Unknown command: ", Cmd/binary>>, Room, Pid, State)
  end,
  State.

find_external_command(Cmd) ->
  {ok, ExtCommands} = application:get_env(scribester_external_commands),
  find_external_command(Cmd, ExtCommands).

find_external_command(_Cmd, []) ->
  {error, notfound};
find_external_command(Cmd, [{ExtCmdName, ExtCmdImpl}|Rest]) ->
  case iolist_to_binary(ExtCmdName) == Cmd of
    true ->
      {ok, ExtCmdImpl};
    false ->
      find_external_command(Cmd, Rest)
  end.

send_message_to_room(Message, Room, Session, _State) ->
  exmpp_session:send_packet(Session,
    exmpp_stanza:set_recipient(exmpp_message:groupchat(Message), Room)).

status_message(#state{logging_enabled=LoggingEnabled}) ->
  Header = <<"Status: ">>,
  LoggingStatus = case LoggingEnabled of
    true -> <<"Logging is on; ">>;
    false -> <<"Logging is off; ">>
  end,
  [Header, LoggingStatus].

run_script(Path, Stdin) ->
  {ok, P, I} = exec:run(Path, [stdin, stdout, stderr, monitor]),
  exec:send(I, iolist_to_binary(Stdin)),
  exec:send(I, eof),
  wait_for_script(P, I, <<>>, <<>>).

wait_for_script(P, I, Stdout, Stderr) ->
  receive
    {stdout, I, Text} ->
      wait_for_script(P, I, [Stdout, Text], Stderr);
    {stderr, I, Text} ->
      wait_for_script(P, I, Stdout, [Stderr, Text]);
    {'DOWN', I, process, P, Reason} ->
      case Reason of
        normal ->
          {ok, Stdout};
        _ ->
          {error, Reason, Stdout, Stderr}
      end
  end.

export_config() ->
  Keys = [scribester_monitored_rooms,
          scribester_timeformat,
          scribester_frontend_enable,
          scribester_timezone,
          scribester_username,
          scribester_text_storage_logdir,
          scribester_json_storage_logdir,
          scribester_resource,
          scribester_special_commands_enabled,
          scribester_server],
  lists:filtermap(fun(K) ->
                      case application:get_env(K) of
                        undefined ->
                          false;
                        {ok, Val} ->
                          {true, {K, format_config_value(K, Val)}}
                      end
                  end, Keys).


format_config_value(scribester_monitored_rooms, Value) ->
  lists:map(fun iolist_to_binary/1, Value);
format_config_value(scribester_timeformat, Value) ->
  iolist_to_binary(Value);
format_config_value(scribester_timezone, Value) ->
  iolist_to_binary(Value);
format_config_value(scribester_username, Value) ->
  iolist_to_binary(Value);
format_config_value(scribester_text_storage_logdir, Value) ->
  iolist_to_binary(Value);
format_config_value(scribester_json_storage_logdir, Value) ->
  iolist_to_binary(Value);
format_config_value(scribester_resource, Value) ->
  iolist_to_binary(Value);
format_config_value(scribester_server, Value) ->
  iolist_to_binary(Value);
format_config_value(_, Value) ->
  Value.
