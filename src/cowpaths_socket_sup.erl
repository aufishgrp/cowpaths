-module(cowpaths_socket_sup).
-behavior(supervisor).

-include("cowpaths.hrl").

%% Supervisor exports
-export([init/1]).

%% API Exports
-export([start_link/0, start_socket/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Supervisor Implementation %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(term()) -> {ok, supervisor:strategy(), non_neg_integer(), non_neg_integer(), [supervisor:child_spec()]} | ignore.
%% @doc Initializes the supervisor.
%% @end
init(_) ->
	{
		ok,
		{
			#{
				strategy  => simple_one_for_one,
				intensity => 100,
				period    => 5
			},
			[
			    #{
			    	id       => cowpaths_socket_sup,
     				start    => {cowpaths_socket, start_link, []},
     				restart  => permanent,
     				shutdown => 1000,
     				type     => worker,
     				modules  => [cowpaths_socket_sup]
     			}
			]
		}
	}.

%%%%%%%%%%%%%%%%%%%%%%%%
%% API Implementation %%
%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link() -> term().
%% @doc Starts the socket supervisor
%% @end
start_link()     -> supervisor:start_link({local, ?COWPATHS_SOCKET_SUP}, ?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%
%% Private Functions %%
%%%%%%%%%%%%%%%%%%%%%%%