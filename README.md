#httpaths

Application that manages configuration of Cowboy.

Allows multiple applications to use a socket(s) without steppig on each others toes. An understanding of how Cowboy routes requests is necessary for understanding httpaths. You should review the documentation here first http://ninenines.eu/docs/en/cowboy/1.0/guide/routing/

##Usage
Start the application.
    
    applicaiton:start(httpaths).
    
###Create a socket to listen for requests

    SocketSpec = #{
        port => integer()               :: Valid port integer, no default
        protocol => http|https|spdy|tls :: Protocol the cowboysocket should use. Corresponds 
                                           to cowboy:start_[protocol]. Default http
        protocol_options => []          :: The Cowboy socket options to use. Dispatch rules may be specified
                                           here but will be overwritten upon calling attach. Default []
        socket_options => []            :: The Socket options to use. Port will be automatically added to this list. 
                                           Default []
        workers => integer()            :: The number of socket acceptors to use. Default 50
    }
    
    httpaths:socket(SocketSpec).
    
{ok, Socket} is returned when a socket is created. {exists, Socket} is returned when the port to use is already being managed.
    
###Attach to the socket
Attach a HostTable to a series of sockets.

    App = any() :: A unique term used to identify routes within the aplication.
    RouteTable = #{
        sockets => all | [integer()] :: The sockets that should handle these specified routes. 
                                        Specified either as a list of port numbers, or the atom 
                                        all if all existing sockets should listen. No default.
        routes  => HostTable         :: No default.
    }
    HostTable = #{
        HostMatch => {PathTable, Constraints} | PathTable
    } :: Remapping of the Cowboy HostSpec.
         [{HostMatch, Constraints, PathsList} | {HostMatch, PathsList}] -> 
         #{HostMatch -> {PathTable, Constraints} | PathTable}
    PathTable = #{
        PathMatch -> {Handler, Opts} | {Handler, Opts, Constraints}
    } :: Remapping of the Cowboy PathsList.
         [{PathMatch, Handler, Opts} | {PathMatch, Constraints, Handler, Opts}] ->
         #{PathMatch -> {Handler, Opts} | {Handler, Opts, Constraints}}
    
    httpaths:attach(App, RouteTable).
    
RouteTables are maintained on a per socket basis.
    
Each time attach is called the provided RouteTable is merged into the existing RouteTable for the sockets specified. As attached is called, the HostMatch constraints specified are merged into the sockets existing HostMatch constraints.
    
Within a socket/HostMatch pairing PathMatch specs must be unique. In the event that a specified PathMatch already exists attach will return {error, {"Duplicate path spec", AttachingApp, AttachedApp}}

###Detach from a socket
Removes all Paths for App from all sockets.

    httpaths:detach(App)

###Check Configuration
Returns the configuration as it is passed to Cowboy.

    httpaths:check_paths().

Build
-----

    $ rebar3 compile
