-module(stockquery_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,start_process/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_ID(Id,Mod,Args, Type), {Id, {Mod, start_link, Args}, permanent, 5000, Type, [Mod]}).
%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->

    Child = [?CHILD(query_manager,worker)],
    {ok, { {one_for_one, 5, 10}, Child} }.

start_process(Index,MOD)->
	Id = erlang:system_time(nano_seconds),
	Child = ?CHILD_ID(Id,query_process,[Index,MOD],worker),
	supervisor:start_child(stockquery_sup,Child).