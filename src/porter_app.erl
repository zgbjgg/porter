%%%----------------------------------------------------------------------
%%%
%%% This module starts the porter application (start link supervisor)
%%%
%%% Created by : Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
%%%----------------------------------------------------------------------

-module(porter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    porter_sup:start_link().

stop(_State) ->
    ok.
