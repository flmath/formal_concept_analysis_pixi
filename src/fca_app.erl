%%%-------------------------------------------------------------------
%% @doc fca public API
%% @end
%%%-------------------------------------------------------------------

-module(fca_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    fca_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
