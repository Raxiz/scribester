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
  {ok, Rooms} = application:get_env(scribester_monitored_rooms),

  Session = exmpp_session:start(),
  JID = exmpp_jid:make(Username, Server, Resource),
  ok  = exmpp_session:auth_basic_digest(Session, JID, Password),
  {ok, _} = exmpp_session:connect_TCP(Session, Server, 5222,
    [{whitespace_ping, 10}]),
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
          look_for_special_command(Body, Room, Pid, State);
        {ok, false} ->
          State
      end;
    false ->
      State
  end;
handle_xmpp_packet(#received_packet{
           packet_type=presence,
           type_attr="unavailable",
           from={_RoomName, _RoomServer, From}}, _Pid,
          State) ->
  {ok, Username} = application:get_env(scribester_username),
  case iolist_to_binary(From) == iolist_to_binary(Username) of
    true ->
      exit(bot_went_unavailable);
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
  send_message_to_room(<<"Unknown command: ", Cmd/binary>>, Room, Pid, State),
  State.

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
