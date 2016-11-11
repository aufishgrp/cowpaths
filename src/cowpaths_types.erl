-module(cowpaths_types).
-include("cowpaths.hrl").

-export_type([
	host/0,           hosts/0,
	portnum/0,
	match/0,
	pathmatch/0,
	handler/0,
	metadata/0,
	constraint/0,     constraints/0,
	constraintspec/0, constraintspecs/0,
	socket/0,         sockets/0,
	path/0,           paths/0,
	cowpath/0,        cowpaths/0,
	trail/0,          trails/0
]).

-type host()            :: iodata().
-type hosts()           :: all | '_' | list(host()).

-type portnum()         :: integer().

-type match()           :: iodata().
-type pathmatch()       :: iodata().
-type handler()         :: module().
-type metadata()        :: map().

-type constraint()      :: {match(), int} | {match(), {module(), atom()}} | {match(), fun((term()) -> true | {true, any()} | false)}.
-type constraints()     :: list(constraint()).

-type constraintspec()  :: constraint() | {iodata(), {module(), atom()}}.
-type constraintspecs() :: list(constraintspec()).

-type socket()          :: portnum() |  #{
	port             => portnum(),
	protocol         => http | https | spdy | tls,
	workers          => integer(),
	protocol_options => list(),
	socket_options   => list()
}.
-type sockets()         :: all | list(socket()).

-type path()            :: #{
	path_match  => pathmatch(),
	constraints => constraintspecs(),
	handler     => handler(),
	options     => any(),
	metadata    => map()
}.
-type paths()           :: list(path()).

-type cowpath()         :: #{
	sockets     => sockets(),
	hosts       => hosts(),
	constraints => constraints(),
	paths       => paths()
}.
-type cowpaths()        :: list(cowpath() | module()).

-type trail()           :: trails:trail().
-type trails()          :: trails:trails().
