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

-spec get_paths() -> cowpaths_types:paths().
get_paths() ->
	get_paths(all).

-spec get_paths(all | atom()) -> cowpaths_types:paths().
get_paths(App) ->
	cowpaths_manager:get_paths(App).

%%====================================================================
%% Internal functions
%%====================================================================
join([], Acc) -> Acc;
join([Cowpath | Cowpaths], Acc) ->
	join(Cowpaths, [Cowpath | Acc]).

-spec expand(cowpaths_types:cowpaths()) -> list(cowpaths_types:cowpath()).
expand(Cowpaths) ->
	lists:foldl(
		fun
			(Cowpath, Acc) when is_atom(Cowpath) ->
				join(expand(Cowpath:cowpaths()), Acc);
			(Cowpath, Acc) ->
				[Cowpath | Acc]
		end,
		[],
		Cowpaths
	)).






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

