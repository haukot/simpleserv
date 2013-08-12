-module(my_http_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% spec types
-type(error() :: term()).
-type(sup_flags() :: term()).
-type(child_spec() :: term()).

%% ===================================================================
%% API functions
%% ===================================================================

-spec(start_link() -> {ok, pid()} | ignore | {error, error()}).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec(init([term()]) -> {ok, {sup_flags(), [child_spec()]}} | ignore | {error, error()}).
init([]) ->
    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent, % permanent | transient | temporary
    Shutdown = 2000,     % brutal_kill | int() >= 0 | infinity
    Type = worker,       % worker | supervisor

    AChild = {'AName', % used to identify the child spec internally by the supervisor
	      {'AModule', start_link, []}, % StartFun = {M, F, A}
	      Restart, Shutdown, Type, 
	      ['AModule']}, % Modules  = [Module] | dynamic

    {ok, {SupFlags, []}}.

