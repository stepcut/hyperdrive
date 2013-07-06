{-# LANGUAGE RankNTypes #-}
module Serve where

import Control.Concurrent         (forkIO)
import Control.Monad.State.Strict (StateT(runStateT))
import Control.Monad.Trans.Error  (ErrorT(runErrorT))
import Control.Monad.Trans        (liftIO)
import Control.Exception          (bracket, finally)
import Pipes                      (Consumer, Producer, Proxy, (>->), hoist, lift, runEffect)
import Pipes.Attoparsec            (ParsingError, isEndOfParserInput)
import Pipes.Parse                (Draw, wrap)
import Pipes.Network.TCP          ( HostPreference(Host), acceptFork, listen
                                  , socketReadS, socketWriteD
                                  )
import Control.Monad              (forever)
import Data.ByteString            (ByteString, empty)
import Network.Socket             (Socket, SockAddr, accept, sClose)
import Request                    (parseRequest)
import Response                   (responseWriter)
import Types                      (Handler, HTTPPipe, Request, Response)

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
    runStateT ( runErrorT  (runEffect $ wrap (hoist (lift . lift) . reader) >-> (httpPipe' secure addr handler) >-> (hoist (lift . lift) . writer) $ ())) []

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
