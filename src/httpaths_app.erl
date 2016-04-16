%%%-------------------------------------------------------------------
%% @doc httpaths public API
%% @end
%%%-------------------------------------------------------------------
-module(httpaths_app).
-behaviour(application).

-include("httpaths.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	ets:new(?HTTPATHS_ROUTES,  [public, named_table]),
	ets:new(?HTTPATHS_SOCKETS, [public, named_table]),

	%% Start app    
    Res = httpaths_sup:start_link(),
    %% Start configured sockets
    lists:foreach(
		fun(Config) ->
			httpaths:listen(Config)
		end,
		application:get_env(httpaths, sockets, [])
	),
    Res.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
