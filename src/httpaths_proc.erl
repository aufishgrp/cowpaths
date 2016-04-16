%%%-------------------------------------------------------------------
%% @doc httpaths process that manages the routing table.
%% @end
%%%-------------------------------------------------------------------
-module(httpaths_proc).

-behaviour(gen_server).

%% API
-export([start_link/0, attach/2, detach/1, socket/1, check_paths/0]).

%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("httpaths.hrl").

-record(httpaths_proc, {}).

%%====================================================================
%% API functions
%%====================================================================
-spec start_link() -> {ok, pid()}.
%% @doc Start the httpaths_proc instance for the node.
%% @end
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec attach(atom(), []) -> ok | {error, term()}.
attach(App, Routes) ->
	gen_server:call(?MODULE, {attach, App, Routes}).

-spec detach(atom()) -> ok | {error, term()}.
detach(App) ->
	gen_server:call(?MODULE, {detach, App}).

-spec socket(#{}) -> ok | {error, term()}.
socket(SocketSpec) -> gen_server:call(?MODULE, {socket, SocketSpec}).

-spec check_paths() -> term().
check_paths() -> gen_server:call(?MODULE, {inspect}).

%%====================================================================
%% GenServer callbacks
%%====================================================================
-spec init(term()) -> gen_spec:gs_init_res(#httpaths_proc{}).
%% @doc Initialize a httpaths_proc.
%% @end
init(_) ->	
	{ok, #httpaths_proc{}}.

-spec handle_call(term(), gen_spec:from(), #httpaths_proc{}) -> gen_spec:gs_call_res(#httpaths_proc{}).
handle_call({socket, SocketSpec}, _From, State) ->
	Socket = case lookup_socket(maps:get(port, SocketSpec)) of
		no_exists ->
			create_socket(SocketSpec);
		Else -> 
			Else
	end,
	{reply, Socket, State};

handle_call({attach, App, #{sockets := Sockets, routes := RouteTable1}}, _From, State) ->
	try
		RouteTables  = get_route_tables(Sockets),
		RouteTables2 = lists:map(
			fun({Socket, RouteTable2}) ->
				{Socket, update_route_table(App, RouteTable1, RouteTable2)}
			end,
			RouteTables
		),
		lists:foreach(
			fun(KV={Socket, RouteTable}) ->
				update_socket(Socket, RouteTable),
				ets:insert(?HTTPATHS_ROUTES, KV)
			end,
			RouteTables2
		),
		{reply, ok, State}
	catch
		Err:Or ->
		{reply, {error,{Err, Or}}, State}
	end;

handle_call({detach, App}, _From, State) ->
	RouteTables  = get_route_tables(all),
	RouteTables2 = lists:map(
		fun({Socket, RouteTable}) ->
			{Socket, filter_route_table(App, RouteTable)}
		end,
		RouteTables
	),
	lists:foreach(
		fun(KV={Socket, RouteTable}) ->
			update_socket(Socket, RouteTable),
			ets:insert(?HTTPATHS_ROUTES, KV)
		end,
		RouteTables2
	),
	{reply, ok, State};

handle_call({inspect}, _From, State) ->
	RouteTables = get_route_tables(all),
	CowboyMap   = lists:map(
		fun({Socket, RouteTable}) ->
			{Socket, fix_route_order(cowboy_router:compile(routetable_to_cowboyspec(RouteTable)))}
		end,
		RouteTables
	),
	{reply, CowboyMap, State};

handle_call(M, _, S) ->
	{reply, {"Unhandled", M}, S}.

-spec handle_cast(term(), #httpaths_proc{}) -> gen_spec:gs_cast_res(#httpaths_proc{}).
%% @doc Handle cast notifications.
%% @end
handle_cast(_, S) -> {noreply, S}.

-spec handle_info(term(), #httpaths_proc{}) -> gen_spec:gs_info_res(#httpaths_proc{}).
%% @doc Handle info notifications.
%% @end
handle_info(_, S) -> {noreply, S}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), #httpaths_proc{}) -> no_return().
%% @doc Perform any last second cleanup.
%% @end
terminate(_, _) -> ok.

-spec code_change({down, term()} | term(), #httpaths_proc{}, term()) -> {ok, #httpaths_proc{}} | {error, term()}.
%% @doc Handle state changes across revisions.
%% @end
code_change(Old, State, Extra) -> httpaths_proc_revisions:migrate(Old, State, Extra).

%%====================================================================
%% Internal functions
%%====================================================================
lookup_socket(Port) ->
	case ets:lookup(?HTTPATHS_SOCKETS, Port) of
		[] -> no_exists;
		[{Port, Socket}] ->{exists, Socket}
	end.

create_socket(SocketSpec) ->
	case httpaths_socket_sup:start_socket(SocketSpec) of
		{ok, Socket} ->
			ets:insert(?HTTPATHS_SOCKETS, {maps:get(port, SocketSpec), Socket}),
			{ok, Socket}
	end.

get_route_tables(all) ->
	get_route_tables(ets:select(?HTTPATHS_SOCKETS, [{{'$1', '_'}, [], ['$1']}]));
get_route_tables(Sockets) ->
	lists:map(
		fun(Socket) ->
			case ets:select(?HTTPATHS_ROUTES, [{{Socket, '_'}, [], ['$_']}]) of
				[]  -> {Socket, #{}};
				[X] -> X
			end
		end,
		Sockets
	).

update_route_table(App, Routes1, RouteTable) ->
	maps:fold(
		fun
			Map(K, {Routes, Constraints}, Acc) ->
				{Routes2, Constraints2} = get_host_info(K, Acc),
				maps:put(K, {update_routes(App, Routes, Routes2), update_constraints(Constraints, Constraints2)}, Acc);
			Map(K, Paths, Acc) -> Map(K, {Paths, []}, Acc)
		end,
		RouteTable,
		Routes1
	).

filter_route_table(App, RouteTable) ->
	maps:map(
		fun(_Host, {PathTable, Constraints}) ->
			{filter_path_table(App, PathTable), Constraints}
		end,
		RouteTable
	).

filter_path_table(App, PathTable) ->
	maps:fold(
		fun
			(_, {App2, _, _, _}, Acc) when App =:= App2 ->
				Acc;
			(Path, Spec, Acc) ->
				maps:put(Path, Spec, Acc)
		end,
		#{},
		PathTable
	).

get_host_info(Host, RouteTable) -> maps:get(Host, RouteTable, {#{}, []}).

update_routes(App, Routes1, Routes2) ->
	maps:fold(
		fun
			Map(K, {Handler, Opts, Constraints}, Acc) ->
				case maps:is_key(K, Acc) of
					true ->
						erlang:error({"Duplicate path spec", K, maps:get(K, Acc)});
					_ -> ok
				end,
				maps:put(K, {App, Handler, Opts, Constraints}, Acc);
			Map(K, {Handler, Opts}, Acc) -> Map(K, {Handler, Opts, []}, Acc)
		end,
		Routes2,
		Routes1
	).

update_constraints(Constraints1, Constraints2) ->
	Constraints1 ++ Constraints2.

update_socket(Socket, RouteTable) ->
	cowboy:set_env(Socket, dispatch, fix_route_order(cowboy_router:compile(routetable_to_cowboyspec(RouteTable)))).

routetable_to_cowboyspec(RouteTable) ->
	lists:map(fun hosttable_to_cowboyspec/1, maps:to_list(RouteTable)).

hosttable_to_cowboyspec({Host, {PathTable, Constraints}}) ->
	{Host, Constraints, pathtable_to_cowboyspec(PathTable)}.

pathtable_to_cowboyspec(PathTable) ->
	lists:map(
		fun({Match, {_, Handler, Options, Constraints}}) ->
			{Match, Constraints, Handler, Options}
		end,
		maps:to_list(PathTable)
	).

fix_route_order(CowboyRoutes) ->
	Order = lists:map(
		fun({Host, Constraints, Paths}) ->
			Paths2 = lists:sort(
				fun({Path1,_,_,_}, {Path2,_,_,_}) ->
					Len1  = length(Path1),
					Len2  = length(Path2),
					Last1 = lists:last(Path1),
					%Last2 = lists:last(Path2),

					%% True if >=
					if
						%% Put longest matches first
						Len1    < Len2  -> false;
						Len1  >   Len2  -> true;
						%% Items ending with a ... need to go last within
						%% a length group.
						Last1 =:= '...' -> false;
						%% Items that are opional go later as well.
						is_atom(Last1)  -> true;
						%% Otherwise we don't care where we are.
						true            -> false
					end
				end,
				Paths
			),
			{Host, Constraints, Paths2}
		end,
		CowboyRoutes
	),
	Order.



