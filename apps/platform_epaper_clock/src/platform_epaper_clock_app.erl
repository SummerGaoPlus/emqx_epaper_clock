%%%-------------------------------------------------------------------
%% @doc platform_epaper_clock public API
%% @end
%%%-------------------------------------------------------------------

-module(platform_epaper_clock_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    platform_epaper_clock_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
