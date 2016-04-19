%%%-------------------------------------------------------------------
%% @doc cowpaths process that manages the routing table.
%% @end
%%%-------------------------------------------------------------------
-module(cowpaths_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, attach/3, detach/1, socket/1, get_paths/0]).

%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("cowpaths.hrl").

-record(cowpaths_manager, {}).

%%====================================================================
%% API functions
%%====================================================================
-spec start_link() -> {ok, pid()}.
%% @doc Start the cowpaths_manager instance for the node.
%% @end
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec attach(term(), any | [], []) -> ok | {error, term()}.
attach(App, Sockets, Routes) ->
	gen_server:call(?MODULE, {attach, App, Sockets, Routes}).

-spec detach(term()) -> ok | {error, term()}.
detach(App) ->
	gen_server:call(?MODULE, {detach, App}).

-spec socket(#{}) -> ok | {error, term()}.
socket(SocketSpec) -> gen_server:call(?MODULE, {socket, SocketSpec}).

-spec get_paths() -> term().
get_paths() -> gen_server:call(?MODULE, {inspect}).

%%====================================================================
%% GenServer callbacks
%%====================================================================
-spec init(term()) -> gen_spec:gs_init_res(#cowpaths_manager{}).
%% @doc Initialize a cowpaths_manager.
%% @end
init(_) ->	
	{ok, #cowpaths_manager{}}.

-spec handle_call(term(), gen_spec:from(), #cowpaths_manager{}) -> gen_spec:gs_call_res(#cowpaths_manager{}).
handle_call({socket, SocketSpec}, _From, State) ->
	Socket = case lookup_socket(maps:get(port, SocketSpec)) of
		no_exists ->
			create_socket(SocketSpec);
		Else -> 
			Else
	end,
	{reply, Socket, State};

handle_call({attach, App, Sockets, Routes0}, _From, State) ->
	try
		Routes1        = trails:compile(Routes0),
		SocketRoutes1  = get_socket_tables(Sockets),
		SocketRoutes2  = lists:map(
			fun({Socket, Routes2}) ->
				{Socket, update_routes(Routes1, Routes2)}
			end,
			SocketRoutes1
		),
		lists:foreach(
			fun(KV={Socket, Routes}) ->
				cowboy:set_env(Socket, dispatch, Routes),
				%% Store SocketTable
				ets:insert(?COWPATHS_ROUTES, KV)
			end,
			SocketRoutes2
		),
		trails:store(Routes0),
		ets:insert(?COWPATHS_PATHS, {App, Sockets, Routes1}),
		{reply, ok, State}
	catch
		error:Or ->
			{reply, {error,Or}, State}
	end;

handle_call({detach, App}, _From, State) ->
	Sockets       = get_sockets_for_app(App),
	Routes1       = get_routes_for_app(App),
	SocketRoutes  = get_socket_tables(Sockets),
	SocketRoutes2 = lists:map(
		fun({Socket, Routes2}) ->
			{Socket, remove_routes(Routes1, Routes2)}
		end,
		SocketRoutes
	),
	lists:foreach(
		fun(KV={Socket, Routes}) ->
			cowboy:set_env(Socket, dispatch, Routes),
			%% Store SocketTable
			ets:insert(?COWPATHS_ROUTES, KV)
		end,
		SocketRoutes2
	),
	{reply, ok, State};

handle_call({inspect}, _From, State) ->
	{reply, get_socket_tables(all), State};

handle_call(M, _, S) ->
	{reply, {"Unhandled", M}, S}.

-spec handle_cast(term(), #cowpaths_manager{}) -> gen_spec:gs_cast_res(#cowpaths_manager{}).
%% @doc Handle cast notifications.
%% @end
handle_cast(_, S) -> {noreply, S}.

-spec handle_info(term(), #cowpaths_manager{}) -> gen_spec:gs_info_res(#cowpaths_manager{}).
%% @doc Handle info notifications.
%% @end
handle_info(_, S) -> {noreply, S}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), #cowpaths_manager{}) -> no_return().
%% @doc Perform any last second cleanup.
%% @end
terminate(_, _) -> ok.

-spec code_change({down, term()} | term(), #cowpaths_manager{}, term()) -> {ok, #cowpaths_manager{}} | {error, term()}.
%% @doc Handle state changes across revisions.
%% @end
code_change(Old, State, Extra) -> cowpaths_manager_revisions:migrate(Old, State, Extra).

%%====================================================================
%% Internal functions
%%====================================================================
lookup_socket(Port) ->
	case ets:lookup(?COWPATHS_SOCKETS, Port) of
		[] -> no_exists;
		[{Port, Socket}] ->{exists, Socket}
	end.

create_socket(SocketSpec) ->
	case cowpaths_socket_sup:start_socket(SocketSpec) of
		{ok, Socket} ->
			ets:insert(?COWPATHS_SOCKETS, {maps:get(port, SocketSpec), Socket}),
			{ok, Socket}
	end.

get_socket_tables(all)     ->
	get_socket_tables(ets:select(?COWPATHS_SOCKETS, [{{'$1', '_'}, [], ['$1']}]));
get_socket_tables(Sockets) ->
	lists:map(
		fun(Socket) ->
			case ets:select(?COWPATHS_ROUTES, [{{Socket, '_'}, [], ['$_']}]) of
				[]  -> {Socket, []};
				[X] -> X
			end
		end,
		Sockets
	).

get_sockets_for_app(App) ->
	case ets:select(?COWPATHS_PATHS, [{{App, '$1', '_'}, [], ['$1']}]) of
		[X] -> X;
		[]  -> []
	end.

get_routes_for_app(App) ->
	case ets:select(?COWPATHS_PATHS, [{{App, '_', '$1'}, [], ['$1']}]) of
		[X] -> X;
		[]  -> []
	end.

update_routes(Routes1, Routes2) ->
	Routes3 = lists:foldl(
		fun
			F(Host = {HostMatch, Constraints, PathsList}, Acc) ->
				case lists:keyfind(HostMatch, 1, Acc) of
					false ->
						[Host | Acc];
					{_, Constraints2, PathsList2} ->
						Host2 = {HostMatch, update_constraints(Constraints, Constraints2), update_pathslists(PathsList, PathsList2)},
						lists:keystore(HostMatch, 1, Routes2, Host2)
				end;
			F({HostMatch, PathsList}, Acc) -> F({HostMatch, [], PathsList}, Acc)
		end,
		Routes2,
		Routes1
	),
	fix_route_order(Routes3).

update_pathslists(PathsList1, PathsList2) ->
	lists:foldl(
		fun
			F(Path = {PathMatch, _, _, _}, Acc) ->
				case lists:keyfind(PathMatch, 1, Acc) of
					false ->
						[Path | Acc];
					Path ->
						%% If the new Path is identical to the one we have
						%%   suppress the reassign error.
						Acc;
					_ ->
						erlang:error({"Path Exists", PathMatch})
				end;
			F(Path, Acc) when is_map(Path) ->
				PathMatch = maps:get(path_match, Path),
				case lists:keyfind(PathMatch, 1, Acc) of
					false ->
						[Path | Acc];
					Path ->
						%% If the new Path is identical to the one we have
						%%   suppress the reassign error.
						Acc;
					_ ->
						erlang:error({"Path Exists", PathMatch})
				end;
			F({PathMatch, Handler, Opts}, Acc) -> F({PathMatch, [], Handler, Opts}, Acc)
		end,
		PathsList2,
		PathsList1
	).

update_constraints(Constraints1, Constraints2) ->
	Constraints1 ++ Constraints2.

remove_routes(Routes1, Routes2) ->
	Routes3 = lists:filtermap(
		fun({HostMatch, Constraints, PathsList1}) ->
			case lists:keyfind(HostMatch, 1, Routes1) of
				false ->
					true;
				{_, _, PathsList2} ->
					case remove_paths(PathsList1, PathsList2) of
						[] -> false;
						PathsList3 -> {true, {HostMatch, Constraints, PathsList3}}
					end
			end
		end,
		Routes2
	),
	fix_route_order(Routes3).

remove_paths(PathsList1, PathsList2) ->
	lists:filtermap(
		fun({PathMatch, _, _, _}) ->
			case lists:keyfind(PathMatch, 1, PathsList1) of
				false -> true;
				_ -> false
			end
		end,
		PathsList2
	).

fix_route_order(Routes) ->
	Order = lists:map(
		fun({HostMatch, Constraints, PathsList}) ->
			PathsList2 = lists:sort(
				fun({PathMatch1,_,_,_}, {PathMatch2,_,_,_}) ->
					{Binary1, Atom1} = lists:splitwith(fun erlang:is_binary/1, PathMatch1),
					{Binary2, Atom2} = lists:splitwith(fun erlang:is_binary/1, PathMatch2),

					LenB1  = length(Binary1),
					LenA1  = length(Atom1),
					LenB2  = length(Binary2),
					LenA2  = length(Atom2),

					LastA1 = case LenA1 of 0 -> undefined; _ -> lists:last(Atom1) end,

					if
						%% Put longest binary matches first
						LenB1 >   LenB2 ->
							true;
						LenB1   < LenB2 ->
							false;
						
						%% If 1 atom and it's '...' A goes after
						LenA1 =:=     1 andalso LastA1 =:= '...' ->
							false;

						%% If A has <= as many atoms as B and A ends in '...' it goes after
						LenA1  =< LenA2 andalso LastA1 =:= '...' ->
							false;

						%% If A has  atoms than B it goes first
						LenA1 >   LenA2 ->
							true;

						%% Otherqise we don't care
						true            ->
							true
					end
				end,
				PathsList
			),
			{HostMatch, Constraints, PathsList2}
		end,
		Routes
	),
	Order.
