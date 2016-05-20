%%%-------------------------------------------------------------------
%% @doc cowpaths API module.
%% @end
%%%-------------------------------------------------------------------
-module(cowpaths).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([attach/3, detach/1, socket/1, get_paths/0]).

%% Trails
-export([trails/0]).

-spec attach(App :: atom(), Sockets :: [] | all, Routes :: []) -> ok.
%% @doc Updates the routes table with the specified Routes for App.
%%      If the app has previously attached the existing rules are overwritten.
%% @end
attach(App, Sockets, Routes) ->
	Routes2 = standardize_routes(Routes),
	cowpaths_manager:attach(App, Sockets, Routes2).

-spec detach(Routes :: []) -> ok.
%% @doc Removes the routes table with the specified routes.
%% @end
detach(Route) ->
	cowpaths_manager:detach(Route).

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
	cowpaths_manager:socket(Spec2).

-spec get_paths() -> [].
get_paths() ->
	cowpaths_manager:get_paths().

%%====================================================================
%% Internal functions
%%====================================================================
standardize_routes(Routes) ->
	lists:flatten(lists:map(fun standardize_route/1, Routes)).

%% Standard cowboy host
standardize_route({HostMatch, Pathslist}) ->
	{HostMatch, standardize_paths(Pathslist)};
%% Trails allows for "easy" config for a single server
%% Allow this for trails and cowboy specs
standardize_route(Path = {_,_,_}) ->
	{'_', [Path]};
standardize_route(Path = {_,_,_,_}) ->
	{'_', [Path]};
standardize_route(Path) when is_map(Path) ->
	{'_', [Path]};
standardize_route(Path) when is_atom(Path) ->
	standardize_routes(trails:trails([Path])).

standardize_paths(Pathslist) ->
	lists:flatten(lists:map(fun standardize_path/1, Pathslist)).

%% Standard cowboy paths
standardize_path(P = {_, _, _})    -> P;
standardize_path(P = {_, _, _, _}) -> P;
%% Trail spec
standardize_path(P) when is_map(P) -> P;
%% Trail module
standardize_path(P) when is_atom(P) -> standardize_paths(trails:trails([P])).

%%====================================================================
%% Unit Test functions
%%====================================================================

start() ->
	ok = application:ensure_started(ranch),
	ok = application:ensure_started(cowlib),
	ok = application:ensure_started(cowboy),
	ok = application:ensure_started(cowpaths).

socket_test() ->
	start(),
	{ok, S1}     = cowpaths:socket(#{port => 8000}),
	{ok, _}      = cowpaths:socket(#{port => 8001}),
	{exists, S1} = cowpaths:socket(#{port => 8000}),
	ok.

attach_single_test() ->
	%% Test assigning to a single socket
	start(),
	_  = cowpaths:socket(#{port => 9000}),
	_  = cowpaths:socket(#{port => 9001}),
	Routes1 = [
		{"Single", [
			{"/Path1",            handler1, []},
			{"/Path1/SubDir", [], handler1, []}
		]}
	],
	ok = cowpaths:attach(app, [9000], Routes1).

attach_error_test() ->
	%% Validate that no error occurs when assigning identical Routes.
	start(),
	_  = cowpaths:socket(#{port => 9000}),
	_  = cowpaths:socket(#{port => 9001}),
	Routes1 = [
		{"Error", [
			{"/Path1",            handler1, []},
			{"/Path1/SubDir", [], handler1, []}
		]}
	],
	ok = cowpaths:attach(app, [9000], Routes1),

	%% Validate that an error occurs when overriding.
	Routes2 = [
		{"Error", [
			{"/Path1",            handler2, []}
		]}
	],
	{error, {"Path Exists", _}} = cowpaths:attach(app, [9000], Routes2).

attach_multi_test() ->
	%% Validate assigning to multiple sockets
	start(),
	_  = cowpaths:socket(#{port => 9000}),
	_  = cowpaths:socket(#{port => 9001}),
	Routes1 = [
		{"Multi", [
			{"/Path1",            handler1, []},
			{"/Path1/SubDir", [], handler1, []}
		]}
	],
	ok = cowpaths:attach(app, [9000, 9001], Routes1).

attach_all_test() ->
	%% Validate assigning to all sockets
	start(),
	_  = cowpaths:socket(#{port => 9000}),
	_  = cowpaths:socket(#{port => 9001}),
	Routes1 = [
		{"All", [
			{"/Path1",            handler1, []},
			{"/Path1/SubDir", [], handler1, []}
		]}
	],
	ok = cowpaths:attach(app, all, Routes1).

detach_single_test() ->
	%% Validate assigning to all sockets
	start(),
	_  = cowpaths:socket(#{port => 9000}),
	_  = cowpaths:socket(#{port => 9001}),
	Routes1 = [
		{"Detach1", [
			{"/Path1",            handler1, []},
			{"/Path1/SubDir", [], handler1, []}
		]}
	],
	ok = cowpaths:attach(detach, all, Routes1),
	ok = cowpaths:detach(detach).

detach_multi_test() ->
	%% Validate assigning to all sockets
	start(),
	_  = cowpaths:socket(#{port => 9000}),
	_  = cowpaths:socket(#{port => 9001}),
	Routes1 = [
		{"DetachX", [
			{"/BadPath",            handler1, []},
			{"/BadPath/SubDir", [], handler1, []}
		]}
	],
	Routes2 = [
		{"DetachX", [
			{"/Path2",            handler1, []},
			{"/Path2/SubDir", [], handler1, []}
		]},
		{"DetachY", [
			{"/Path2",            handler1, []},
			{"/Path2/SubDir", [], handler1, []}
		]}
	],
	ok = cowpaths:attach(detach1, all, Routes1),
	ok = cowpaths:attach(detach2, [9001], Routes2),
	ok = cowpaths:detach(detach1).

trails() ->
	Metadata = #{
		get => #{
			tags => ["example"],
			description => "Example description.",
			produces => ["text/plain"]
		}
	},
	[trails:trail("/description", trails_handler, [], Metadata)].

trails2() ->
	Metadata = #{
		get => #{
			tags => ["example"],
			description => "Example description.",
			produces => ["text/plain"]
		}
	},
	[trails:trail("/description", trails_handler, [], Metadata)].

trails3() ->
	Metadata = #{
		get => #{
			tags => ["example"],
			description => "Example description.",
			produces => ["text/plain"]
		}
	},
	[trails:trail("/description", trails_handler, [], Metadata), {"/path5", handler, []}].

trails_test() ->
	%% Validate assigning to all sockets
	application:set_env(cowpaths, swagger, 9000),
	start(),
	_  = cowpaths:socket(#{port => 9000}),
	_  = cowpaths:socket(#{port => 9001}),
	ok = cowpaths:attach(trails_test, all, [cowpaths]),
	ok = cowpaths:attach(trails_test, all, trails2()),
	ok = cowpaths:attach(trails_test, all, trails3()).





