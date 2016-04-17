%%%-------------------------------------------------------------------
%% @doc cowpaths public API
%% @end
%%%-------------------------------------------------------------------
-module(cowpaths_app).
-behaviour(application).

-include("cowpaths.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	ets:new(?COWPATHS_PATHS,   [public, named_table]),
	ets:new(?COWPATHS_ROUTES,  [public, named_table]),
	ets:new(?COWPATHS_SOCKETS, [public, named_table]),

	%% Start app    
    Res = cowpaths_sup:start_link(),
    %% Start configured sockets
    lists:foreach(
		fun(Config) ->
			cowpaths:listen(Config)
		end,
		application:get_env(cowpaths, sockets, [])
	),
    Res.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
