{-# LANGUAGE RankNTypes #-}
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
import Control.Monad.State.Strict (StateT(runStateT))
import Control.Monad.Trans.Error  (ErrorT(runErrorT))
import Control.Monad.Trans        (MonadIO(liftIO))
import Control.Exception          (bracket, finally)
import Data.Void                  (Void)
import Pipes                      (Consumer, Producer, Proxy, (>->), hoist, lift, runEffect)
import Pipes.Attoparsec           (ParsingError, parseMany)
import Pipes.Parse                ()
import Pipes.Network.TCP          {- ( HostPreference(Host), acceptFork, listen
                                  , socketReadS, socketWriteD
                                  ) -}
import Control.Monad              (forever)
import Data.ByteString            (ByteString, empty)
import Network.Socket             (Socket, SockAddr, accept, sClose)
import Request                    (pRequest)
import Response                   (responsePipe)
import Types                      (Handler, HTTPPipe, Request, Response)

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

serve :: String -- ^ port number to listen on
       -> Proxy () (Int, Request) () Response IO (Either (ParsingError, Producer ByteString IO ()) ())
      -> IO ()
serve port handler =
    listen (Host "127.0.0.1") port $ \(listenSocket, listenAddr) ->
        serveSocket listenSocket handler

serveSocket :: MonadIO m =>
               Socket
            -> Proxy () (Int, Request) () Response IO (Either (ParsingError, Producer ByteString IO ()) ())
            -> m b

serveSocket listenSocket handler =
    forever $
      acceptFork listenSocket $ \(acceptedSocket, clientAddr) ->
         let reader = fromSocket acceptedSocket 4096
             writer = toSocket acceptedSocket
         in do e <- runHTTPPipe reader writer False clientAddr handler
--               print e
               return ()


runHTTPPipe
  :: MonadIO m =>
     Producer ByteString m r
     -> Proxy
          ()
          ByteString
          ()
          Void
          m
          (Either (ParsingError, Producer ByteString m r) r)
     -> Bool
     -> SockAddr
     -> Proxy
          ()
          (Int, Request)
          ()
          Response
          m
          (Either (ParsingError, Producer ByteString m r) r)
     -> m (Either (ParsingError, Producer ByteString m r) r)
runHTTPPipe producer consumer secure clientAddr handler =
    runEffect $ httpPipe producer secure clientAddr handler >-> consumer

httpPipe :: MonadIO m =>
            Producer ByteString m r
         -> Bool
         -> SockAddr
         -> Proxy () (Int, Request) () Response m (Either (ParsingError, Producer ByteString m r) r)
         -> Proxy a' a () ByteString m (Either (ParsingError, Producer ByteString m r) r)
httpPipe producer secure clientAddr handler =
    (parseMany (pRequest secure clientAddr) producer) >-> handler >-> responsePipe
------------------------------------------------------------------------------
-- new
------------------------------------------------------------------------------

