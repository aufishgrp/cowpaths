-module(cowpaths_types).
-include("cowpaths.hrl").

-export_type([
	host/0,           hosts/0,
	port_number/0,
	match/0,
	handler/0,
	metadata/0,
	constraint/0,     constraints/0,
	socket/0,         sockets/0,
	path/0,           paths/0,
	cowpath/0,        cowpaths/0
]).

-type host()            :: iodata().
-type hosts()           :: all | list(host()).

-type port_number()     :: integer().

-type match()           :: iodata().
-type handler()         :: module().
-type metadata()        :: map().

-type constraint()      :: {match(), integer()} | {match(), {module(), atom()}} | {match(), fun((term()) -> true | {true, any()} | false)}.
-type constraints()     :: list(constraint()).

-type socket()          :: port_number() |  #{
	port             => port_number(),
	protocol         => http | https | spdy | tls,
	workers          => integer(),
	protocol_options => list(),
	socket_options   => list()
}.
-type sockets()         :: all | list(socket()).

-type path()            :: #{
	path_match  => match(),
	constraints => constraints(),
	handler     => handler(),
	options     => any(),
	metadata    => metadata()
}.
-type paths()           :: list(path()).

-type cowpath()         :: #{
	sockets     => sockets(),
	hosts       => hosts(),
	constraints => constraints(),
	paths       => paths()
}.
-type cowpaths()        :: list(cowpath() | module()).
