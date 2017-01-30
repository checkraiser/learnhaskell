# User space threads

- An implementation of a thread abstraction inside a single process (or on top of a small number of operating system threads, then the programmer gets to write his/her application using the concurrency primitives provided by the language, and the user-space threads implementation will provide the low-level time-sharing and I/O multiplexing support.

# Web server

A web server can be thought of as two components: an implementation of the HTTP protocol, and a top-level loop that continually accepts new connections and initiates new transactions.

# Main loop

- Server opens a listening socket using `listenOn`, which returns an abstract value of type `Socket`.

    listenOn :: PortId -> IO Socket

- We can wait for new connection request on this socket using `accept` function

    accept :: Socket -> IO (Handle, HostAddress)

`accepts` waits until a connection request is received, then returns a `Handle` for communication with the remote host, and the address of the remote host.

- Main loop:

`acceptConnections` takes a server configuration of type `Config` and a listening socket, then calls `accept` to wait for a new connection request on the socket. When a connection request is received, a new worker thread is forked using `forkIO`, and the main loop goes back to waiting for connections.

    acceptConnections :: Config -> Socket -> IO ()
    acceptsConnections conf sock = do
      (handle, host_addr) <- accept sock
      forkIO $ catch (talk conf handle host_addr `finally` hClose handle) (\e -> logError e)
      acceptConnections conf sock

- The worker thread calls `talk` which is the main function for communicating in HTTP with a client. The interesting part here is what happens if an exception is raised during `talk`. The `finnaly` combinator allows strict sequencing to be specified, independent of exceptions:

    finally :: IO a -> IO b -> IO a

- The call to `talk` is also enclosed in a `catch` combinator:

    catch :: IO a -> (Exception -> IO a) -> IO a

This combinator performs its first argument, and if an exception is raised, passes it to the second argument (the exception handler), otherwise it returns the result. In contrast to finally, catch specifies an action to be performed only when an exception is raised, whereas finally specifies an action which is always to be executed.

# HTTP Protocol implementation

