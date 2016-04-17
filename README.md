#cowpaths

Application that manages configuration of Cowboy.

Allows multiple applications to use a socket(s) without steppig on each others toes.

##Usage
Start the application.
    
    applicaiton:start(cowpaths).
    
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
    
    cowpaths:socket(SocketSpec).
    
{ok, Socket} is returned when a socket is created. {exists, Socket} is returned when the port to use is already being managed.
    
###Attach to the socket
Attach a HostTable to a series of sockets.

    App = any() :: A unique term used to identify routes within the aplication.
    Sockets = [integer()] | all :: A list of ints corresponding to the ports that have been bound by
                                   calling cowpaths:socket(SocketSpec) or the atom all. If all is passed
                                   the rules are applied to all 
    Routes = [] :: Cowboy routes as described here: http://ninenines.eu/docs/en/cowboy/1.0/guide/routing/
    cowpaths:attach(App, Sockets, Routes).
    
RouteTables are maintained on a per socket basis.
    
Each time attach is called the provided RouteTable is merged into the existing RouteTable for the sockets specified. As attached is called, the HostMatch constraints specified are merged into the sockets existing HostMatch constraints.
    
Within a socket/HostMatch pairing PathMatch specs must be unique. In the event that a specified PathMatch already exists attach will return {error, {"Path exists", PathMatch}}

###Detach from a socket
Removes all Routes for App from all sockets.

    cowpaths:detach(App).

###Check Configuration
Returns the configuration as it is passed to Cowboy.

    cowpaths:get_paths().

Build
-----

    $ rebar3 compile
