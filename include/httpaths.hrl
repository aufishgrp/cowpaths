-define(HTTPATHS_SUP,        httpaths_sup).
-define(HTTPATHS_PROC,       httpaths_proc).
-define(HTTPATHS_ROUTES,     httpaths_routetable).
-define(HTTPATHS_PROC_RR,    httpaths_proc_rr).
-define(HTTPATHS_SOCKET,     httpaths_socket).
-define(HTTPATHS_SOCKET_SUP, httpaths_socket_sup).
-define(HTTPATHS_SOCKETS,    httpaths_sockets).

-record(httpath, {
	spec        = undefined :: binary(),
	options     = []        :: [term()] | {term(), term(), term()},
	controller  = undefined :: atom(),
	metadata    = #{}       :: map(),
	constraints = []        :: [binary()]
}).

-record(httpaths, {
	app       = undefined :: atom(),
	hosts     = all       :: [string()] | all,
	sockets   = all       :: [string()] | all,
	routes    = []        :: [map()]    | trails
}).
