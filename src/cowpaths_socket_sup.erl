-module(cowpaths_socket_sup).
-behavior(supervisor).

-include("cowpaths.hrl").

%% Supervisor exports
-export([init/1]).

%% API Exports
-export([start_link/0, add_socket/1, start_socket/1]).

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
     				start    => {cowpaths_socket_sup, start_socket, []},
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
start_link()     -> supervisor:start_link({local, ?COWPATHS_SOCKET_SUP}, ?MODULE, []).

add_socket(Args) -> supervisor:start_child(?COWPATHS_SOCKET_SUP, Args).

start_socket(#{
	port             := Port,
	protocol         := Protocol,
	protocol_options := ProtocolOptions,
	socket_options   := SocketOptions,
	workers          := Workers
}) ->
	Fun = case Protocol of
		http  -> start_http;
		https -> start_https;
		spdy  -> start_spdy;
		tls   -> start_tls
	end,

	%% Ensure that we have dispatch rules configured
	UProtocolOptions = case lists:keyfind(env, 1, ProtocolOptions) of
		false ->
			[{env, [{dispatch, []}]} | ProtocolOptions];
		{_, Env} ->
			case lists:keyfind(dispatch, 1, Env) of
				false ->
					lists:keystore(env, 1, ProtocolOptions, {env, [{dispatch, []} | Env]});
				_ ->
					ProtocolOptions
			end
		end,

	USocketOptions = lists:keystore(port, 1, SocketOptions, {port, Port}),
	cowboy:Fun(Port, Workers, USocketOptions, UProtocolOptions).