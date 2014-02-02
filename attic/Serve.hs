{-# LANGUAGE RankNTypes, OverloadedStrings #-}
{-

This module is intended to provide a composable API that allows the
developer to put together their own accept loops, add metrics, use
transport layers like SSL, create fake transport layers for testing,
listen on a specific socket, etc.

In the common case the user wants to listen on a specific port and possible a specific address (localhost, everything, etc).

We also have to handle IPv4 vs IPv6.

If the user wants TLS then the same conditions apply, plus they need readers/writers that handle TLS.

In general the reader and writer will be matched. For example, reading
and writing from the same socket with the same compression style.

In theory someone might want to use an unmatched pair for some reason.


  do (reader, writer) <- initSocket
     reader /> runHTTP /> writer


-}
module Serve where

import Control.Concurrent         (forkIO)
import Control.Monad.State.Strict (StateT(runStateT), get, evalStateT)
import Control.Monad.Trans.Error  (ErrorT(runErrorT))
import Control.Monad.Trans        (MonadIO(liftIO))
import Control.Exception          (bracket, finally)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import Data.Void                  (Void)
import Pipes                      (Consumer, Consumer', Producer, Producer', Pipe, Proxy, (>->), hoist, lift, runEffect, await, yield)
import Pipes.Attoparsec           (ParsingError, parseMany, parse)
import Pipes.Lift                 (evalStateP)
import Pipes.Network.TCP          
import qualified Pipes.Prelude    as P
import Pipes.Parse                (input)
import Pipes.Safe                 (runSafeT)
import Control.Monad              (forever)
import Data.ByteString            (ByteString, empty)
import Network.Socket             (Socket, SockAddr, sClose)
import qualified Network.Socket   as NS
import Request                    (pRequestHead)
import Response                   (responsePipe, statusLine, renderHeaders)
import Types                      (Handler, HTTPPipe, Method(..), Request(..), Response(..))


------------------------------------------------------------------------------
-- serve
------------------------------------------------------------------------------
{-
-- | listen on a port and handle 'Requests'
serve :: String -- ^ port number to listen on
      -> Handler IO -- ^ handler
      -> IO ()
serve port handler =
    listen (Host "127.0.0.1") port $ \(listenSocket, listenAddr) ->
        serveSocket listenSocket httpPipe handler

-- | listen on a port and handle 'Requests'
serve' :: String -- ^ port number to listen on
       -> HTTPPipe
       -> Handler IO -- ^ handler
       -> IO ()
serve' port httpPipe' handler =
    listen (Host "127.0.0.1") port $ \(listenSocket, listenAddr) ->
        serveSocket listenSocket httpPipe' handler

------------------------------------------------------------------------------
-- internals
------------------------------------------------------------------------------

serveSocket :: Socket     -- ^ socket to listen on
            -> HTTPPipe
            -> Handler IO -- ^ handler
            -> IO ()
serveSocket listenSocket httpPipe' handler =
    forever $
      acceptFork listenSocket $ \(acceptedSocket, clientAddr) ->
         let reader = recv acceptedSocket 4096
             writer = send acceptedSocket
         in do e <- runHTTPPipe False clientAddr reader writer httpPipe' handler
--               print e
               return ()

-- | this is where we construct the pipe that reads from the socket,
-- processes the request, and sends the response
runHTTPPipe :: Bool     -- ^ is this an HTTPS connection
            -> SockAddr -- ^ ip of the client
            -> (() -> Producer ByteString IO ())
            -> (() -> Consumer ByteString IO ())
            -> HTTPPipe
            -> Handler IO -- ^ handler
            -> IO (Either ParsingError (), [ByteString])
runHTTPPipe secure addr reader writer httpPipe' handler =
    runStateT ( runErrorT  (runEffect $  (hoist (lift . lift) . reader) >-> (httpPipe' secure addr handler) >-> (hoist (lift . lift) . writer) $ ())) []


httpPipe ::HTTPPipe
httpPipe secure addr handler () = go
    where
      go = do eof <- hoist lift $ isEndOfParserInput
--              liftIO $ putStrLn "Not eof"
              if eof
               then return ()
               else do req  <- parseRequest secure addr
--                       liftIO $ print req
                       resp <- handler req
                       responseWriter resp
                       go
-}

{-
runHTTPPipe :: Bool     -- ^ is this an HTTPS connection
            -> SockAddr -- ^ ip of the client
            -> (() -> Producer ByteString IO ())
            -> (() -> Consumer ByteString IO ())
            -> HTTPPipe
            -> Handler IO -- ^ handler
            -> IO (Either ParsingError (), [ByteString])
    runStateT ( runErrorT  (runEffect $  (hoist (lift . lift) . reader) >-> (httpPipe' secure addr handler) >-> (hoist (lift . lift) . writer) $ ())) []
-}
{-
serve :: String -- ^ port number to listen on
      -> Pipe (Int, Request) Response IO (Either (ParsingError, Producer ByteString IO ()) ())
      -> IO ()
serve port handler =
    listen (Host "127.0.0.1") port $ \(listenSocket, listenAddr) ->
        serveSocket listenSocket handler

serveSocket :: MonadIO m =>
               Socket
            -> Pipe (Int, Request) Response IO (Either (ParsingError, Producer ByteString IO ()) ())
            -> m b
serveSocket listenSocket handler =
    forever $
      acceptFork listenSocket $ \(acceptedSocket, clientAddr) ->
         let reader = fromSocket acceptedSocket 4096
             writer = toSocket   acceptedSocket
         in do e <- runHTTPPipe reader writer False clientAddr handler
               return ()

runHTTPPipe :: MonadIO m =>
               Producer ByteString m r
            -> Consumer ByteString m (Either (ParsingError, Producer ByteString m r) r)
            -> Bool
            -> SockAddr
            -> Pipe (Int, Request) Response m (Either (ParsingError, Producer ByteString m r) r)
            -> m (Either (ParsingError, Producer ByteString m r) r)
runHTTPPipe producer consumer secure clientAddr handler =
    runEffect $ httpPipe producer secure clientAddr handler >-> consumer

httpPipe :: MonadIO m =>
            Producer ByteString m r
         -> Bool
         -> SockAddr
         -> Pipe (Int, Request) Response m (Either (ParsingError, Producer ByteString m r) r)
         -> Producer ByteString m (Either (ParsingError, Producer ByteString m r) r)
httpPipe producer secure clientAddr handler =
    (parseMany (pRequest secure clientAddr) producer) >-> handler >-> responsePipe
-}

{-

We want the user to be able to hook into the pipeline and do things
such as decide when to work a new thread to handle each request or add
rate limiting or maybe just live monitoring.

We also want to be able to plug in different functions for
reading/writing data to the network (so we can support http, https,
spdy, or even just fake it for testing).

we also want to be able to plug in different code for doing the
parsing serialization.

and then the user needs to be able to supply their handlers.


fromSocket >-> parseRequest >-> acceptLoop handler >-> serializeResponse >-> toSocket

parseRequest :: Pipe ByteString Request

fromSocket

-}

{-

We start by listening on a socket. That is a blocking action.

When a client connects, we can now accept the socket and starting processing one or more requests for that client.

Any responses must be sent back over that same accepted socket.

The traditional method is to fork a new thread to handle each accepted socket. But there are other options available.

In theory we could have a single handler than handles all requests from any accepted socket, one at a time.

acceptor :: Socket -> Producer m Socket r
forkomatic :: (Socket -> m r) -> Consumer m Socket r

-}

-- FIXME: how do we ensure the socket gets closed?
acceptor :: (MonadIO m) =>
            Socket
         -> Producer (Socket, SockAddr) m r
acceptor listenSocket =
  forever $
    do conn@(csock,_) <- liftIO (NS.accept listenSocket)
       yield conn

runSocket :: (MonadIO m) => Int -> (Socket, SockAddr) -> (SockAddr -> Pipe ByteString ByteString m ()) -> m ()
runSocket bytes (socket, sockAddr) pipe =
    runEffect $ fromSocket socket 4096 >-> pipe sockAddr >-> toSocket socket

runSocketConsumer :: (MonadIO m) =>
                     Int
                  -> (SockAddr -> Pipe ByteString ByteString m ())
                  -> Consumer' (Socket, SockAddr) m ()
runSocketConsumer bytes pipe =
    forever $
      do (socket, sockAddr) <- await
         fromSocket socket 4096 >-> pipe sockAddr >-> toSocket socket

{-

Should the handler be able to await more than one request or yield more than one response?

In the normal case, that can not happen -- the closest that can happen is the 100-Continue which yields one or more responses.

Should the handler even be a pipe?

AT the core, the handler could be, Request -> IO Response

Though, we have the issue of making sure the entire body is consumed exactly once. No more -- no less. Even in the case of early termination?


It might seem like having one straight pipeline would be neat:

    acceptor >-> fromSocket >-> parseRequests >-> handleRequests >-> printResponses >-> toSocket

but, we start to see the cracks almost immediately.

fromSocket and toSocket need to actually reference the accepted socket. Also, how do we know that handleRequests is producing responses 1-to-1 and in order? Also, what happens if we want to use multiple threads and cores?

Instead of one big pipeline, we likely need multiple forms of composition.



pipeline :: (MonadIO m) => Pipe (Int, Request) Response m a -> Pipe (Producer ByteString m (), SockAddr) ByteString m a
pipeline handler = requestPipe >-> handler >-> responsePipe
-}

forkomatic :: (MonadIO m) => ((Socket, SockAddr) -> IO ()) -> Consumer (Socket, SockAddr) m r
forkomatic handler =
  forever $
    do s <- await
       _tid <- liftIO $ forkIO $ handler s
       return ()

-- parseRequest secure clientAddr = parseMany (pRequest secure clientAddr)

-- pipeline listenSocket handler =
--     acceptor listenSocket >-> forkomatic (\s@(sock, clientAddr) -> return ()) -- runEffect $ (requestProducer clientAddr (fromSocket sock 4096) >-> handler s >-> toSocket sock))

-- duku producer handler (_, clientAddr)  =
--  parseMany (pRequest False clientAddr) producer >-> handler >-> responsePipe

fromSocketProducer :: (MonadIO m) => Pipe (Socket, SockAddr) (Producer ByteString m (), SockAddr) m r
fromSocketProducer =
    forever $
      do (sock, clientAddr) <- await
         yield (fromSocket sock 4096, clientAddr)

requestLoop :: Bool -> SockAddr -> (Request -> IO ()) -> StateT (Producer ByteString IO ()) IO ()
requestLoop secure clientAddr handler =
  forever $
    do erq <- parse (pRequestHead secure clientAddr)
       case erq of
         (Left e) -> error $ show e
         (Right (bytesRead, rq)) ->
             do liftIO $ print rq
                p <- get
                let rqBody' :: Producer ByteString IO ()
                    rqBody' =
                        case rqMethod rq of
                          POST -> case lookup "Content-Length" (rqHeaders rq) of
                                    Nothing       -> return ()
                                    (Just bodyLen) ->
                                        p >-> P.take (read $ C.unpack bodyLen)
                          _ -> return ()
                    rq' = rq { rqBody = rqBody' }
                liftIO $ handler rq'

serveHTTP :: String -- ^ port number to listen on
      -> (Request -> IO Response)
      -> IO ()
serveHTTP port handler =
    listen (Host "127.0.0.1") port $ \(listenSocket, listenAddr) ->
        forever $
          acceptFork listenSocket $ \(acceptedSocket, clientAddr) ->
              do let reader :: Producer ByteString IO ()
                     reader = fromSocket acceptedSocket 4096

                     writer :: Consumer ByteString IO ()
                     writer = toSocket acceptedSocket

                     responseWriter :: Response -> IO ()
                     responseWriter res =
                         runEffect $ (do yield $ B.concat [ statusLine (rsCode res)
                                                          , renderHeaders (rsHeaders res)
                                                          , "\r\n"
                                                          ]
                                         rsBody res
                                         liftIO $ yield "\r\n"
                                     ) >-> writer
                 evalStateT (requestLoop False clientAddr (\req -> handler req >>= responseWriter)) reader

hello =
    serveHTTP "8000" (\rq -> let body = "PONG"
                                 res = Response { rsCode = 200
                                                , rsHeaders = [("Content-Lenth", C.pack (show (B.length body)))]
                                                , rsBody = yield body
                                                }
                             in return res)


{-
requestPipe :: (Monad m) => Pipe (Producer ByteString m (), SockAddr) (Int, Request) m r
requestPipe =
    forever $
    do (p, clientAddr) <- await
       parseMany (pRequest False clientAddr) p
-}





{-

We have the problem that the producer and the consumer at either end of the Effect both need to share a socket.

Maybe the socket should go into a reader?

singleThread listenSocket handler =
    acceptor listenSocket >-> fromSocketProducer >-> pipeline >-> toSocket 
-}


-- requestProducer clientAddr producer =
--    parseMany (pRequest False clientAddr)
{-
server port handler =
    listen (Host "127.0.0.1") port $ \(listenSocket, listenAddr) ->
        pipeline listenSocket handler

pong =
    do let body = "PONG"
           res = Response { rsCode    = 200
                          , rsHeaders = [("Content-Length", C.pack (show (B.length body)))]
                          , rsBody    = yield body
                          }
       yield res
       pong


main :: IO ()
main = runSafeT $ runEffect $ server "8000" pong
-}