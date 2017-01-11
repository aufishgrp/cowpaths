-module(cowpaths_socket).

-export([start_socket/1, parse_config/1]).

-spec start_socket(cowpaths_types:socket()) -> term().
%% @doc Starts a socket
%% @end
start_link(#{
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
	cowboy:Fun(Port, Workers, USocketOptions, UProtocolOptions);
start_link(SocketConfig) ->
	start_link(parse_config(SocketConfig)).

-spec parse_config(cowpaths_types:socket()) -> cowpaths_types:socket().
%% @doc Generates a complete socket from the values given.
%% @end
parse_config(Int) when is_integer(Int) ->
	Socket = socket_defaults(),
	Socket#{
		port => Int
	};
parse_config(Map) when is_map(Map) ->
	lists:foldl(fun update_socket/2, socket_defaults(), maps:to_list(Map)).

-spec update_socket({atom(), any()}, cowpaths_types:socket()) -> cowpaths_types:socket().
%% @doc Updates the field of the socket.
%% @end
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

-spec socket_defaults() -> map().
%% @doc Returns the default values for socket values that may be ommitted.
%% @end
socket_defaults() ->
	#{
		protocol         => http,
		workers          => 50,
		protocol_options => [],
		socket_options   => []
	}.
