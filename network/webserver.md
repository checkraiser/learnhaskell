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

Serving a request is a simple pipeline:

1. Read the request from the socket
2. Parse the request
3. Generate the response
4. Send the response back to the client
5. If the connection is to be kept alive, return to step 1

Reading the request from socket is perormed by `getRequest`

    getRequest :: Handle -> IO [String]

which takes file handle representing the socket on which communication with the client is taking place, and returns a list of strings, each one being a single line of the request. A typical request looks something like this:

    GET /index.html HTTP/1.1
    Host: www.haskell.org
    Date: Wed May 31 11:08:40 GMT 2017

The first line gives the command (GET), the name of the object requested, and the version of the HTTP protocol being used by the client. Subsequent lines, termed `headers`, give additional information and are mostly optional. The server is required to ignore any headers it doesn't understand.

The next stage is to parse the request into a `Request`:

    data Request = Request {
      reqCmd :: RequestCmd, 
      reqURI :: ReqURI,
      reqHTTPVer :: HTTPVersion,
      reqHeaders :: [RequestHeader]
    }

Requests are parsed by `parseRequest`:

    parseRequest :: Config -> [String] -> Either Response Request

Next, generate the response:

    data Response = Response {
      respCode :: Int,
      respHeaders :: [ResponseHeader],
      respCoding :: [TransferCoding],
      respBody :: ResponseBody,
      respSendBody :: Bool
    }

    data ResponseBody
      = NoBody
      | FileBody Integer{-size-} FilePath
      | HTMLBody HTML

   genResponse :: Config -> Request -> IO Response

`genResponse` performs a number of checks on the validity of the request, and generates an appropriate response. A valid GET request will result in a response with a FileBody. An invalid request will result in an error response of some description. If we later extend the server to support dynamically generated pages for example, then the ResponseBody datatype could be extended with urther constructors for different types of content.

The final step is to send the response to the client:

    sendResponse :: Config -> Handle -> Response -> IO ()

Pulling all this together, the top-level `talk` function looks like this:

    talk :: Config -> Handle -> HostAddress -> IO ()
    talk conf handle host_addr = do
      strs <- getRequest handle
      case parseRequest strs of
        Left resp -> sendResponse conf handle resp
        Right req -> do
          resp <- genResponse conf req
          sendResponse conf handle resp
          logAccess req resp host_addr
          if (isKeepAlive req)
            then talk conf handle host_addr
            else return ()

In reality, there is some extra code to deal with catching, and logging of errors and timeouts.

The call to `logAccess` causes an entry to be written to the log file describing the transaction.

# Timeouts

A web server needs some form of timeout mechanism, so that clients which hang or take an inordinately long time to respond can be disconnected, and the resources associated with the connection freed back to the system.

    timeout :: Int -- timeout in seconds
            -> IO a -- action to run
            -> IO a -- action to run on timeout
            -> IO a

The application `timeout t a b` should behave as follows: `a` is run until it either completes, or `t` seconds passes. If it completes in time `t`, `timeout` returns the result immediately, otherwise `a` is terminated with an exception and `b` is executed. If a or b raises an exception, then the exception will be propagated by `timeout`.

# Logging

A web server normally produces log files listing all the requests made and certain information about the response sent by the server.

- The time the request was received
- The requestor's address,
- The URL requested
- The response code
- The number of bytes transered
- The time taken for the request to comlete
- The client software's type & version
- The referring URL

    logAccess :: Request -> Response -> HostAddress -> TimeDiff -> IO ()

The actual generation of the log entries and writing of log entries to the file is done by a separate thread. Worker threads communicate with the logging subsystem via a global unbounded channel, by calling `logAccess`. The logging thread removes items from the channel and manufactures log entries which are then written to a log file.


# Global variables

Global variables let you avoid the loss of modularity and performance that results rom passing around extra parameters that are rarely needed.

A global `MVar` can be defined in Haskell as:

    global_mvar :: MVar String
    globalmvar = unsafePerformIO newEmptyMVar

We still access the MVar using the standar `putMVar` and `takeMVar` operations.

We use global mutable variables in the web server in the following places:

- To store the channels by which the worker threads can communicate with the logging threads.
- To store the `ThreadIds` of the logging threads, so that the main thread can send them an exception to restart them.
- To store the command line options. The program is only started once, so this is a write-once mutable variable. 

# Runtime configuration

Our web server is configured by editing a text file, in a similar way to other popular web servers. When the server starts up, it parses the configuration file, and if there are no errors found, immedicately starts serving requests.

In our server, we take the approach that the new configuration should only take effect for new connections, and existing connections should be allowed to continue using the old settings.
The configuration is passed into the worker thread when it is created, so thwn the configuration changes all we need to do is ensure that any new threads receive the new configuration.

The approach we took is to send the main thread an asynchronous exception when it should re-read the configuration file.

- A signal on Unix-like operating systems. This is the traditional way to kick a process into re-reading its configuration file, and consits of sending a process a signal from the command line. This method is implemented in our web server as follows: the incoming signal causes a new thread to start, which immediately sends an exception to the main thread. The main thread catches the signal and re-reads the configuration file.

