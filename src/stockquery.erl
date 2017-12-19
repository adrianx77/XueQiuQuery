-module(stockquery).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0]).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    code:add_path("ebin"),
    Paths = filelib:wildcard("./deps/*/ebin"),
    RunPath = code:get_path(),
    lists:foreach(
        fun(Path) ->
            case lists:member(Path, RunPath) of
                false -> true = code:add_path(Path);
                true ->
                    ignor
            end
        end, Paths),
    application:ensure_all_started(inets),
    stockquery_sup:start_link().

stop(_State) ->
    ok.

start()->
    application:start(?MODULE).