%%%----------------------------------------------------------------------
%%%
%%% This module is for eunit test [porter app]
%%%
%%% Created by : Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
%%%----------------------------------------------------------------------

-module(porter_sup_test).
-include_lib("eunit/include/eunit.hrl").

-define(CLIENTID, <<"clientid">>).
-define(CONNECT, <<"connect-">>).
-define(DISCONNECT, <<"disconnect-">>).
-define(CONN_ALIVE, <<"connection:keep alive">>).
-define(CONN_DEAD, <<"connection:dead">>).
-define(PORT, 2070).

porter_sup_start_test() ->
    application:load(porter),
    application:set_env(porter, port, ?PORT),
    application:set_env(porter, connect, ?CONNECT),
    application:set_env(porter, disconnect, ?DISCONNECT),
    ok = application:start(porter),
    Apps = application:which_applications(),
    ?assertEqual(true, lists:keymember(porter,1,Apps)),
    ok = application:stop(porter).

porter_connect_user_test_() ->
    {setup,
      fun setup/0,
      fun cleanup/1,
      {timeout, 60, 
	?_test(
	    begin
		{ok, Socket} = connect(),
                Message = reply(Socket),
                ?assertEqual(?CONN_ALIVE, Message),
		{ok, Clients} = porter:keep_alive(),
		?assertEqual(true, lists:keymember(?CLIENTID,1,Clients))
	    end)}}.	

porter_disconnect_user_test_() ->
    {setup,
      fun setup/0,
      fun cleanup/1,
      {timeout, 60,
        ?_test(
            begin
		{ok, Socket} = connect(),
		Message = reply(Socket),
		?assertEqual(?CONN_ALIVE, Message),
		{ok, Clients} = porter:keep_alive(),
		?assertEqual(true, lists:keymember(?CLIENTID,1,Clients)),
		ok = disconnect(Socket),
		Messaged = reply(Socket),
		?assertEqual(?CONN_DEAD, Messaged),
		Client = porter:keep_alive(?CLIENTID),
                ?assertEqual({ok, undefined}, Client)
            end)}}.

porter_send_message_test_() ->
    {setup,
      fun setup/0,
      fun cleanup/1,
      {timeout, 60,
        ?_test(
            begin
                {ok, Socket} = connect(),
		Message = reply(Socket),
		?assertEqual(?CONN_ALIVE, Message),
		{ok, _} = porter:send_msg(?CLIENTID, <<"message1">>),
		IncommingMsg1 = reply(Socket),
		?assertEqual(<<"message1">>, IncommingMsg1),
		{ok, _} = porter:send_msg(?CLIENTID, <<"message2">>),
		IncommingMsg2 = reply(Socket),
		?assertEqual(<<"message2">>, IncommingMsg2)
            end)}}.

%% =============================================================================
%% Auxiliary and internal functions
%% -----------------------------------------------------------------------------

setup() ->
    application:load(porter),
    application:set_env(porter, port, 2070),
    application:set_env(porter, connect, ?CONNECT),
    application:set_env(porter, disconnect, ?DISCONNECT),
    application:start(porter).

cleanup(_) ->
    application:stop(porter).

%%--------------------------------------------------------------------
%% connect a client to porter server
connect() ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, true}, {reuseaddr, true}]),
    ok = gen_udp:send(Socket, "127.0.0.1", 2070, "connect-clientid"),
    {ok, Socket}.

%%--------------------------------------------------------------------
%% disconnect a client present in porter server
disconnect(Socket) ->
    ok = gen_udp:send(Socket, "127.0.0.1", 2070, "disconnect-clientid").

%%--------------------------------------------------------------------
%% receive messages from porter server
reply(Socket) ->
    receive {udp, Socket, _, _, Msg} -> Msg end.	    
