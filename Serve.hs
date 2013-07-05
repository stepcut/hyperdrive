{-# LANGUAGE RankNTypes #-}
module Serve where

import Control.Concurrent        (forkIO)
import Control.Exception         (bracket, finally)
import Pipes
import Pipes.Parse
import Pipes.Network.TCP
-- import Control.Proxy             (Pipe, Producer, Consumer, ProxyFast, Server, Client, (>->), (<-<), runProxy, idT, request)
-- import Control.Proxy.TCP         (HostPreference(Host), acceptFork, listen, socketReadS, socketWriteD)
-- import Control.Proxy.Trans       (liftP)
-- import Control.Proxy.Trans.State (StateP, evalStateK)
import Control.Monad             (forever)
import Data.ByteString           (ByteString, empty)
import Data.Monoid
import Network.Socket            (Socket, SockAddr, accept, sClose)
-- import ParseRequestHand          (pipeBody, parseRequest)
import Request
-- import Control.Proxy.Parse.Commit       (ParseFailure)
import Response                  (responseWriter, responseWriter')
import Types                     (Handler, HTTPPipe, Request, Response)
import Control.Monad.Trans.Either
import Pipes.Attoparsec (ParsingError)

------------------------------------------------------------------------------
-- serve
------------------------------------------------------------------------------

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
         let reader = socketReadS 4096 acceptedSocket
             writer = socketWriteD acceptedSocket
         in do e <- runHTTPPipe False clientAddr reader writer httpPipe' handler
               print e
               return ()

-- | this is where we construct the pipe that reads from the socket,
-- processes the request, and sends the response
{-
runHTTPPipe :: Bool     -- ^ is this an HTTPS connection
            -> SockAddr -- ^ ip of the client
            -> (() -> Producer ByteString IO ())
            -> (() -> Pipe ByteString ByteString IO ())
            -> HTTPPipe
            -> Handler IO -- ^ handler
            -> IO ()
-}
runHTTPPipe secure addr reader writer httpPipe' handler =
    runStateT ( runEitherT  (runEffect $ wrap (hoist (lift . lift) . reader) >-> (httpPipe secure addr handler) >-> (hoist (lift . lift) . writer) $ ())) []
{-
httpPipe :: HTTPPipe
httpPipe secure addr handler = evalStateK empty $ httpPipe' secure addr handler

-- | and this is the real heart of things
httpPipe' :: Bool -- ^ is this an HTTPS connection
         -> SockAddr
         -> Handler IO
         -> (() -> StateP ByteString ProxyFast () ByteString () ByteString IO b)
httpPipe' secure addr handler () =
    forever $
      do request  <- parseRequest secure addr
         response <- ((liftP . (const $ handler request)) <-< pipeBody request) ()
         liftP $ responseWriter (response :: Response IO)
         return ()

httpPipe2 :: HTTPPipe
httpPipe2 secure addr handler () =
    do request <- P.parseRequest secure addr ()
       resp <- handler request
       responseWriter resp
       return ()

httpPipe3 :: HTTPPipe
httpPipe3 secure addr handler () =
    forever $
      do eRequest <- P.pr''' secure addr ()
         case eRequest of
          (Left err) -> error $ show err
          (Right request) ->
              do resp <- handler request
                 responseWriter resp
o                 return ()

-}

-- httpPipe :: (Monad m) => Bool -> SockAddr -> a -> () -> Pipe ByteString ByteString m a

httpPipe :: Monad m =>
            Bool
         -> SockAddr
         -> (() -> Proxy () Request () (Response) (EitherT ParsingError (StateT [ByteString] m)) ())
         -> (() -> Proxy Draw (Maybe ByteString) () ByteString (EitherT ParsingError (StateT [ByteString] m)) ())
httpPipe secure addr handler =
    parseRequest secure addr >-> handler >-> responseWriter
{-
    do request <- P.parseRequest secure addr  ()
       resp <- handler request
       responseWriter resp
       return ()
-}

{-
httpPipe2 secure addr handler =
    do req <- P.parseRequest secure addr ()
       resp <- handler req
       responseWriter' resp
       return ()
-}