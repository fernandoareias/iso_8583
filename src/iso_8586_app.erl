%%%-------------------------------------------------------------------
%% @doc iso_8586 public API
%% @end
%%%-------------------------------------------------------------------

-module(iso_8586_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    iso_8586_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
