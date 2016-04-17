%%%-------------------------------------------------------------------
%% @doc cowpaths top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(cowpaths_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	Supervisor = #{
		strategy  => one_for_one,
		intensity => 5,
		period    => 30
	},

	Children = [
		#{
			id       => cowpaths_manager,
			start    => {cowpaths_manager, start_link, []},
			restart  => permanent,
			shutdown => 5000,
			type     => worker,
			modules  => [cowpaths_manager]
		},
		#{
			id       => cowpaths_socket_sup,
			start    => {cowpaths_socket_sup, start_link, []},
			restart  => permanent,
			shutdown => 5000,
			type     => supervisor,
			modules  => [cowpaths_socket_sup]	
		}
	],

    {ok, {Supervisor, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
