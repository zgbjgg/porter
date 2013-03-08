%%%----------------------------------------------------------------------
%%%
%%% This module implements an erlang client that connects to the socket 
%%% server for send & receive messages from/to it
%%%
%%% Created by : Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
%%%----------------------------------------------------------------------

-module(client).
-export([connect/0]).
-define(ERLANG, <<"connect:erlang">>).

connect() ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, true}, {reuseaddr, true}]),
    ok = gen_udp:send(Socket, "127.0.0.1", 2070, ?ERLANG),
    client_loop(Socket).

client_loop(Socket) ->
     receive
         {udp, Socket, _, _, Msg} ->
             io:format("Incomming message: ~w~n",[Msg]),
             client_loop(Socket)
    end.
