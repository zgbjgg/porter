%%%----------------------------------------------------------------------
%%%
%%% Include for porter app
%%%
%%% Created by : Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
%%%----------------------------------------------------------------------

-define(DISCONNECT, <<"disconnect">>).
-define(CONNECT, <<"connect">>).
-define(TIMESTAMP(Year, Mon, Day, Hour, Min, Sec), lists:flatten(io_lib:fwrite("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Year,Mon,Day,Hour,Min,Sec]))).
-define(PORT_UDP_OPTIONS, [binary, {active, true}]).
