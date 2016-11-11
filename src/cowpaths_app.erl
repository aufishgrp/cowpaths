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
			ok = valid_socket(cowpaths:socket(Config))
		end,
		application:get_env(cowpaths, sockets, [])
	),
    case application:get_env(cowpaths, swagger, undefined) of
    	undefined ->
    		ok;
    	Port ->
    		ok = valid_socket(cowpaths:socket(#{port => Port})),
    		ok = cowpaths:attach(cowpaths_swagger, [Port], [cowboy_swagger_handler])
    end,
    Res.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
valid_socket(ok)          -> ok;
valid_socket({exists, _}) -> ok;
valid_socket(X)           -> X.
