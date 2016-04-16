%%%-------------------------------------------------------------------
%% @doc httpaths API module.
%% @end
%%%-------------------------------------------------------------------
-module(httpaths).

%% API
-export([attach/2, detach/1, socket/1, check_paths/0, t/0]).

%%====================================================================
%% API functions
%%====================================================================

-spec attach(App :: atom(), RouteTable :: []) -> ok.
%% @doc Updates the routes table with the specified Routes for App.
%%      If the app has previously attached the existing rules are overwritten.
%% @end
attach(App, Route) ->
	httpaths_proc:attach(App, Route).

-spec detach(Routes :: []) -> ok.
%% @doc Removes the routes table with the specified routes.
%% @end
detach(Route) ->
	httpaths_proc:detach(Route).

-spec socket(Spec :: #{}) -> ok | {error, term()}.
%% @doc Creates a new socket based on the specification given.
%%      Spec :: #{
%%	        port             => integer(),
%%          protocol         => http | https | spdy | tls, ## Default http
%%          workers          => integer(),                 ## Default 50
%%          protocol_options => [],                        ## []
%%          socket_options   => []                         ## []
%%      }
%% @end
socket(Spec) ->
	Spec2 = #{
		port             => maps:get(port,     Spec),
		protocol         => maps:get(protocol, Spec, http),
		protocol_options => maps:get(protocol_options, Spec, []),
		socket_options   => maps:get(socket_options,   Spec, []),
		workers          => maps:get(workers, Spec, 50)
	},
	httpaths_proc:socket(Spec2).

-spec check_paths() -> [].
check_paths() ->
	httpaths_proc:check_paths().

t() ->
	%application:start(sasl),
	ok = application:start(ranch),
	ok = application:start(cowlib),
	ok = application:start(cowboy),
	ok = application:start(httpaths),

	httpaths:socket(#{port => 9000}),
	httpaths:socket(#{port => 9001}),
	httpaths:attach(apple, #{
		sockets => [9000],
		routes  => #{
			"1" => #{
				"/1/[page/:number]" => {handle1, [opts1]},
				"/1/1"              => {handle2, [opts1]},
				"/1/1/1"            => {handle3, [opts1]},
				"/1/1/1/..."        => {handle4, [opts1]}
			}
		}
	}),
	httpaths:attach(banana, #{
		sockets => all,
		routes  => #{
			"1" => #{
				"/2/2/2"        => {handle3, [opts1]}
			},
			"2" => #{
				"/2/2/2"        => {handle3, [opts1]}
			}
		}
	}).












