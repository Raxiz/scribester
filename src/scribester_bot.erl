-module(scribester_bot).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Records
%% ------------------------------------------------------------------

-record(state, {
    session,
    logging_enabled
  }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  {ok, Username} = application:get_env(scribester_username),
  {ok, Server} = application:get_env(scribester_server),
  {ok, Password} = application:get_env(scribester_password),
  {ok, Resource} = application:get_env(scribester_resource),
  {ok, Rooms} = application:get_env(scribester_monitored_rooms),

  Session = exmpp_session:start(),
  JID = exmpp_jid:make(Username, Server, Resource),
  ok  = exmpp_session:auth_basic_digest(Session, JID, Password),
  {ok, _} = exmpp_session:connect_TCP(Session, Server, 5222),
  {ok, _} = exmpp_session:login(Session),
  exmpp_session:send_packet(Session,
                    exmpp_presence:set_status(exmpp_presence:available(), "")),
  [join_room(Session, R) || R <- Rooms],
  {ok, #state{session=Session, logging_enabled=true}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(#received_packet{} = Packet, State) ->
  %% io:format("~p~n", [Packet]),
  NState = handle_xmpp_packet(Packet, State),
  {noreply, NState};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

join_room(Session, Room) ->
  {ok, Username} = application:get_env(scribester_username),
  exmpp_session:send_packet(Session,
    exmpp_stanza:set_recipient(exmpp_presence:available(),
                               Room ++ "/" ++ Username)),
  ok. %TODO: find a way to see if join have failed.

handle_message_in_room(Room, From, Msg, Time,
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
          raw_packet=Raw
       }, State) when From /= undefined ->
  Room = [RoomName, "@", RoomServer],
  Body = exmpp_message:get_body(Raw),
  Time = extract_timestamp(Raw),
  handle_message_in_room(Room, From, Body, Time, State),
  case application:get_env(scribester_special_commands_enabled) of
    {ok, true} ->
      look_for_special_command(Body, Room, State);
    {ok, false} ->
      State
  end;
handle_xmpp_packet(_, State) ->
  State.

extract_timestamp(Raw) ->
  Delay = exmpp_xml:get_element(Raw, 'urn:xmpp:delay', delay),
  Stamp = exmpp_xml:get_attribute(Delay, <<"stamp">>, undefined),
  case Stamp of
    undefined ->
      calendar:universal_time();
    _ ->
      iso8601:parse(iolist_to_binary(Stamp))
  end.

look_for_special_command(Body, Room, State) ->
  {ok, Username} = application:get_env(scribester_username),
  Prefix = <<(iolist_to_binary(Username))/binary, " ">>,
  PrefixSize = size(Prefix),
  case Body of
    <<Prefix:PrefixSize/binary, Rest/binary>> ->
      handle_special_command(Rest, Room, State);
    _ ->
      State
  end.

handle_special_command(<<"off">>, Room, State) ->
  send_message_to_room(<<"Logging off">>, Room, State),
  State#state{logging_enabled=false};

handle_special_command(<<"on">>, Room, State) ->
  send_message_to_room(<<"Logging on">>, Room, State),
  State#state{logging_enabled=true};

handle_special_command(<<"status">>, Room, State) ->
  send_message_to_room(status_message(State), Room, State),
  State;

handle_special_command(Cmd, Room, State) ->
  send_message_to_room(<<"Unknown command: ", Cmd/binary>>, Room, State),
  State.

send_message_to_room(Message, Room, #state{session=Session}) ->
  exmpp_session:send_packet(Session,
    exmpp_stanza:set_recipient(exmpp_message:groupchat(Message), Room)).

status_message(#state{logging_enabled=LoggingEnabled}) ->
  Header = <<"Status: ">>,
  LoggingStatus = case LoggingEnabled of
    true -> <<"Logging is on; ">>;
    false -> <<"Logging is off; ">>
  end,
  [Header, LoggingStatus].
