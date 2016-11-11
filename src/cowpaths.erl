%%%-------------------------------------------------------------------
%% @doc cowpaths API module.
%% @end
%%%-------------------------------------------------------------------
-module(cowpaths).
-include("cowpaths.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([attach/1, attach/2, detach/1, get_paths/0]).

-spec attach(Cowpaths :: cowpaths_types:cowpaths()) -> ok.
%% @doc Updates the routes tables with the specified Routes for App.
%%      If the app has previously attached the existing rules are overwritten.
%% @end
attach(Cowpaths) ->
	attach(application:get_application(), Cowpaths).

-spec attach(App :: atom(), Cowpaths :: cowpaths_types:cowpaths()) -> ok.
%% @doc Updates the routes tables with the specified Routes for App.
%%      If the app has previously attached the existing rules are overwritten.
%% @end
attach(App, Cowpaths) ->
	Cowpaths = parse_cowpaths(Cowpaths),
	listen_and_attach(App, Cowpaths).

-spec detach(App :: atom()) -> ok | {error, term()}.
%% @doc Removes the routes for the specified App.
%% @end
detach(App) ->
	cowpaths_manager:detach(App).

-spec get_paths() -> [].
get_paths() ->
	cowpaths_manager:get_paths().

%%====================================================================
%% Internal functions
%%====================================================================
expand(Cowpaths) ->
	lists:flatten(lists:map(
		fun
			(Cowpath) when is_atom(Cowpath) ->
				Cowpath:cowpaths();
			(Cowpath) ->
				Cowpath
		end,
		Cowpaths
	)).

-spec parse_cowpaths(cowpaths_types:cowpaths()) -> list().
parse_cowpaths(Cowpaths) ->
	Cowpaths1 = expand(Cowpaths),
	lists:map(fun parse_cowpath/1, Cowpaths1).

-spec parse_cowpath(cowpaths_types:cowpath() | module()) -> list().
parse_cowpath(Cowpath) when is_map(Cowpath) ->
	Sockets      = parse_sockets(maps:get(sockets, Cowpath, undefined)),
	Hosts        = parse_hosts(maps:get(hosts, Cowpath, undefined)),
	Constraints  = parse_constraints(maps:get(constraints, Cowpath, undefined)),
	Trails       = parse_trails(maps:get(paths, Cowpath, undefined)),
	Cowboy       = trails_to_cowboy(Trails),

	TrailRoutes  = lists:map(
		fun(Host) ->
			{Host, Trails}
		end,
		case Hosts of '_' -> ['_']; _ -> Hosts end
	),

	CowboyRoutes = lists:map(
		fun(Host) ->
			{Host, Constraints, Cowboy}
		end,
		case Hosts of '_' -> ['_']; _ -> Hosts end
	),

	{Sockets, TrailRoutes, CowboyRoutes}.

-spec parse_sockets(cowpaths_types:sockets()) -> cowpaths_types:sockets().
parse_sockets(all)       -> all;
parse_sockets(Sockets)   ->
	lists:map(fun parse_socket/1, Sockets).

-spec parse_socket(cowpaths_types:socketspec()) -> cowpaths_types:socket().
parse_socket(Int) when is_integer(Int) ->
	Socket = socket_defaults(),
	Socket#{
		port => Int
	};
parse_socket(Map) when is_map(Map) ->
	lists:foldl(fun update_socket/2, socket_defaults(), maps:to_list(Map)).

update_socket({port, Value}, Socket) when is_integer(Value) ->
	Socket#{port => Value};
update_socket({protocol, Value}, Socket) when Value =:= http orelse Value =:= https orelse
                                              Value =:= spdy orelse Value =:= tls ->
	Socket#{protocol => Value};
update_socket({workers, Value}, Socket) when is_integer(Value) andalso Value > 0 ->
	Socket#{workers => Value};
update_socket({protocol_options, Value}, Socket) when is_list(Value) ->
	Socket#{protocol_options => Value};
update_socket({socket_options, Value}, Socket) when is_list(Value) ->
	Socket#{socket_options => Value};
update_socket(Option, _) ->
	erlang:error(invalid_config_value, [Option]).

socket_defaults() ->
	#{
		protocol         => http,
		workers          => 50,
		protocol_options => [],
		socket_options   => []
	}.

-spec parse_hosts(cowpaths_types:hostspecs()) -> cowpaths_types:hosts().
parse_hosts('_')   -> '_';
parse_hosts(all)   -> '_';
parse_hosts(Hosts) when is_list(Hosts) ->
	lists:map(fun parse_host/1, Hosts).

-spec parse_host(cowpaths_types:hostspec()) -> cowpaths_types:host().
parse_host(Host) when not is_atom(Host) -> Host.

-spec parse_constraints(cowpaths_types:constraintspecs()) -> cowpaths_types:constraints().
parse_constraints(ConstraintSpecs) when is_list(ConstraintSpecs) ->
	lists:map(fun parse_constraint/1, ConstraintSpecs).

-spec parse_constraint(cowpaths_types:constraintspec()) -> cowpaths_types:constraint().
parse_constraint({_, int} = Constraint) ->
	Constraint;
parse_constraint({_, Fun} = Constraint) when is_function(Fun) ->
	Constraint;
parse_constraint({Match, {Module, Fun}}) when is_atom(Module) andalso is_atom(Fun) ->
	Fun = fun(X) ->
		Module:Fun(X)
	end,
	{Match, Fun}.

-spec parse_trails(cowpaths_types:paths()) -> cowpaths_types:trails().
parse_trails(PathSpecs) ->
	lists:map(fun parse_trail/1, PathSpecs).

-spec parse_trail(cowpaths_types:path()) -> cowpaths_types:trail().
parse_trail(Path) when is_map(Path) ->
	lists:foldl(fun update_trail/2, trails_defaults(), maps:to_list(Path)).

update_trail({path_match, PathMatch}, Path) ->
	Path#{path_match => PathMatch};
update_trail({constraints, Constraints}, Path) when is_list(Constraints) ->
	Path#{constraints => parse_constraints(Constraints)};
update_trail({handler, Handler}, Path) when is_atom(Handler) ->
	Path#{handler => Handler};
update_trail({options, Options}, Path) ->
	Path#{options => Options};
update_trail({metadata, MetaData}, Path) when is_map(MetaData) ->
	Path#{metadata => MetaData}.

trails_defaults() ->
	#{
		constraints => [],
		options     => [],
		metadata    => #{}
	}.

trails_to_cowboy(Trails) ->
	lists:map(fun trail_to_cowboy/1, Trails).

trail_to_cowboy(#{
	path_match  := PathMatch,
	handler     := Handler,
	constraints := Constraints,
	options     := Options
}) ->
	{PathMatch, Constraints, Handler, Options}.

valid_socket(ok)          -> ok;
valid_socket({exists, _}) -> ok;
valid_socket(X)           -> X.

listen_and_attach(_, []) -> ok;
listen_and_attach(App, [{Sockets, Trails, Cowboy} | Specs]) ->
	{ok, SocketIds} = cowpaths_manager:sockets(App, Sockets),
	cowpaths_manager:attach(App, SocketIds, Trails, Cowboy).	

%%====================================================================
%% Unit Test functions
%%====================================================================

socket_parse_test() ->
	#{
		port             := 80,
		protocol         := http,
		workers          := 50,
		protocol_options := [],
		socket_options   := []
	} = A = parse_socket(80),

	[A, A] = parse_sockets([80, 80]),

	[#{
		port             := 8001,
		protocol         := https,
		workers          := 5000,
		protocol_options := [option1],
		socket_options   := [options1]
	}] = parse_sockets([
		#{
			port             => 8001,
			protocol         => https,
			workers          => 5000,
			protocol_options => [option1],
			socket_options   => [options1]
		}
	]),
	[#{
		port             := 8001,
		protocol         := http,
		workers          := 50,
		protocol_options := [],
		socket_options   := []
	}] = parse_sockets([
		#{
			port => 8001
		}
	]).

host_parse_test() ->
	'_' = parse_hosts('_'),
	'_' = parse_hosts(all),
	["Match1", "Match2"] = parse_hosts(["Match1", "Match2"]).

cowpaths_to_trails_test() ->
	Path1 = #{
path_match  => "/match1/option1",
constraints => [{"match1", int}],
handler     => handler1,
options     => [],
metadata    => #{}
},

	Cowpath1 = #{
		sockets     => all,
		hosts       => all,
		constraints => [],
		paths       => [
			Path1
		]
	},

	Cowpath2 = #{
		sockets     => all,
		hosts       => all,
		constraints => [{"hello", int}],
		paths       => [
			Path1
		]
	},

	[{_, Trails, Cowboy}] = parse_cowpaths([Cowpath1]),

	A = trails:compile(Trails),
	B = cowboy_router:compile(Cowboy),
	A = B,
	{A, B},

	[{_, Trails2, Cowboy2}] = parse_cowpaths([Cowpath2]),

	io:format("Trails\n~p\nCowboy\n~p\n", [Trails2, cowboy_router:compile(Cowboy2)]),

	[
		{H1,[{"hello", int}],P1}
	] = cowboy_router:compile(Cowboy2),
	[
		{H1,_,P1}
	] = trails:compile(Trails2),

	{A, B}.

