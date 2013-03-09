%%%----------------------------------------------------------------------
%%%
%%% This module starts the socket and keep the alive connections.
%%% Also it provides methods to send & receive messages 
%%%
%%% Created by : Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
%%%----------------------------------------------------------------------

-module(porter).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, keep_alive/0, keep_alive/1, send_msg/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3,set_structure_msg/1]).

-define(SERVER, ?MODULE).

-include("porter.hrl").

-record(state, {lsocket, keep_alive, disconnect, connect}).

%%%===================================================================
%%% API
%%%===================================================================

keep_alive() ->
    keep_alive('null').

keep_alive(Id) ->
    gen_server:call(?MODULE, {keep_alive, Id}).

send_msg(Id, Msg) ->
    gen_server:call(?MODULE, {send_msg, Id, Msg}).

set_structure_msg(NewMsg) ->
    gen_server:call(?MODULE, {set_structure_msg, NewMsg}).

stop() ->
    gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    {ok, Port} = application:get_env(porter, port),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Port]) ->
    {ok, Socket} = gen_udp:open(Port, ?PORT_UDP_OPTIONS),
    {ok, Msgd} = application:get_env(porter, disconnect),
    {ok, Msgc} = application:get_env(porter, connect),
    io:format("~s [porter] up and running at: ~p\n", [timestamp(), Socket]),
    {ok, #state{lsocket=Socket, keep_alive=[], disconnect=Msgd, connect=Msgc}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({set_structure_msg, NewMsg}, _From, _State=#state{lsocket=Socket, keep_alive=Keep, disconnect=Msgd, connect=Msgc}) ->     
    NewMsgc =  case  proplists:get_value(connect, NewMsg,'null') of 
               'null'  ->  Msgc;
                Valc    -> Valc
               end,
    NewMsgd =  case proplists:get_value(disconnect, NewMsg,'null') of
               'null'  ->  Msgd;
                Vald    -> Vald
               end,
    {reply, {ok, {NewMsgc,NewMsgd}}, #state{lsocket=Socket, keep_alive=Keep, disconnect=NewMsgd, connect=NewMsgc}};         
handle_call({send_msg, Id, Msg}, _From, State=#state{lsocket=Socket, keep_alive=Keep, disconnect=_Msgd, connect=_Msgc}) ->
    Routing = {Host, Port} = proplists:get_value(Id, Keep),
    _Repl = gen_udp:send(Socket, Host, Port, Msg),
    {reply, {ok, Routing}, State};
handle_call({keep_alive, 'null'}, _From, State=#state{lsocket=_Socket, keep_alive=Keep, disconnect=_Msgd, connect=_Msg}) ->
    {reply, {ok, Keep}, State};
handle_call({keep_alive, Id}, _From, State=#state{lsocket=_Socket, keep_alive=Keep, disconnect=_Msgd, connect=_Msgc}) ->
    {reply, {ok, proplists:get_value(Id, Keep)}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.
	    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%% the message is for the first time that someone connect to socket, then
%% process the message, SIMPLE: IF THE USER IS IN KEEP ALIVE DISCONNECT, OTHERWISE CONNECT !!
handle_info({udp, _SocketIn, Host, Port, Incomming}, #state{lsocket=Socket, keep_alive=Keep, disconnect=Msgd, connect=Msgc}) ->
    [Msgds, Msgcs] = [ erlang:size(B) * 8  || B <- [Msgd, Msgc] ],
    State = case Incomming of
	        <<Msgc:Msgcs/bitstring, Id/binary>> ->
	    	    io:format("~s [porter] connected: ~p\n", [timestamp(), Id]),
		    _Repl = gen_udp:send(Socket, Host, Port, <<"connection:keep alive">>),
		    #state{lsocket=Socket, keep_alive=[ {Id, {Host, Port}}| Keep ], disconnect=Msgd, connect=Msgc};
		<<Msgd:Msgds/bitstring, Id/binary>> ->		
    		    io:format("~s [porter] disconnected: ~p\n", [timestamp(), Id]),
    		    #state{lsocket=Socket, keep_alive=proplists:delete(Id, Keep), disconnect=Msgd, connect=Msgc};
		_                                   ->
		    io:format("~s [porter] unhandled message: ~p\n", [timestamp(), Incomming]),
		    #state{lsocket=Socket, keep_alive=Keep, disconnect=Msgd, connect=Msgc}
	    end,
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

timestamp() ->
    {Hour,Min,Sec} = time(),
    {Year,Mon,Day} = date(),
    ?TIMESTAMP(Year, Mon, Day, Hour, Min, Sec).
