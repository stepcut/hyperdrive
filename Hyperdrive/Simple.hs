{-# LANGUAGE OverloadedStrings, RankNTypes, RecordWildCards, OverloadedStrings, ScopedTypeVariables #-}
{-

Simple implementations for parsing Requests and generating Responses.

Right now there is no attempt to make them correct. Mostly they are
just dummy implementations so that we can work out the rest of the API
and then later drop in a solid parser.

-}
module Hyperdrive.Simple where

import Control.Applicative        ((*>), pure, many)
import Control.Concurrent         (threadDelay)
import Control.Monad.Catch        (MonadCatch)
import Control.Monad              (forever, forM)
import Control.Monad.Trans        (MonadIO(liftIO))
import Control.Concurrent         (forkFinally)
import Control.Concurrent.Async   (async, waitCatch)
import Control.Concurrent.SSem    (SSem)
import Control.Concurrent.SSem    as SSem
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception              as E
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Data.ByteString (ByteString)
import qualified Data.ByteString  as B
import Data.CaseInsensitive       (mk, original)
import Data.String                (fromString)
import Hyperdrive.FakeParser      (fakeParseRequest)
import Hyperdrive.Serve           (RequestParser, handleOne)
import Hyperdrive.Types           (Request(..), RequestBodyLength(..)
                                  , Response(..), ResponseBody(..))
import Network.HTTP.Types         (HeaderName, ResponseHeaders, Status(..)
                                  , HttpVersion(..), status200)
import Network.Socket             (Socket, SockAddr(..), close)
import Network.Simple.TCP         (HostPreference(..), ServiceName(..))
import qualified Network.Socket   as N
import qualified Network.Simple.TCP as NS
import Pipes                      (Producer, Producer', Consumer', Effect, (>~), (>->), each, await, runEffect, lift, yield, Proxy)
import Pipes.Concurrent           (Buffer(Bounded), spawn', Input(..), Output(..), performGC)
import qualified Pipes.Prelude    as P
import qualified Pipes.Attoparsec as Pa
import qualified Pipes.ByteString as Pb
import qualified Pipes.Concurrent as Pc
import Pipes.Network.TCP.Safe     (serve, listen, accept, acceptFork, toSocket,fromSocket)
import qualified Pipes.Parse      as Pp
import Pipes.Safe                 (runSafeT)
import qualified Pipes.Safe       as Ps

------------------------------------------------------------------------------
-- Response
------------------------------------------------------------------------------

renderStatusLine :: Status -> ByteString
renderStatusLine (Status 200 msg) =
    "HTTP/1.1 200 OK\r\n"

renderResponseHead :: Response m -> ByteString
renderResponseHead Response{..} =
    B.concat [ renderStatusLine _rsStatus
             , renderResponseHeaders _rsHeaders
             , "\r\n"
             ]

renderHeader :: (HeaderName, ByteString) -> ByteString
renderHeader (n, v) =
    B.concat [ original n , ": ", v, "\r\n"]

renderResponseHeaders :: ResponseHeaders -> ByteString
renderResponseHeaders headers =
    B.concat $ map renderHeader headers

responseProducer :: (Monad m) =>
                    Response m
                 -> Producer ByteString m ()
responseProducer res =
    do yield $ renderResponseHead res
       case _rsBody res of
         (ResponseProducer p) -> p


stdoutResponse :: (MonadIO m) =>
                  Response m
               -> Pp.Parser ByteString m (Maybe e)
stdoutResponse res =
    (lift $ runEffect $ (responseProducer res) >-> Pb.stdout) >> return Nothing

------------------------------------------------------------------------------
-- serve
------------------------------------------------------------------------------

ppRequest :: (Monad m) =>
             RequestParser Pa.ParsingError m
ppRequest = Pa.parse (fakeParseRequest False)

simpleOne :: (Functor m, MonadIO m) =>
             RequestParser Pa.ParsingError m
          -> (Response m -> Pp.Parser ByteString m (Maybe Pa.ParsingError))
          -> (Request -> Pp.Parser ByteString m (Either Pa.ParsingError (Response m)))
          -> Pp.Parser ByteString m (Maybe Pa.ParsingError, Bool)
simpleOne parseRequest sendResponse handler =
    handleOne parseRequest (\res -> sendResponse res) handler

data InOutClose e m =
    InOutClose { inByteString :: Producer ByteString m ()
               , outResponse  :: Response m -> Pp.Parser ByteString m (Maybe e)
               , closeIt      :: m ()
               }

socketResponse :: (MonadIO m) =>
                  Socket
               -> Response m
               -> Pp.Parser ByteString m (Maybe e)
socketResponse csock res =
    do -- liftIO $ putStrLn "sending response..."
       e <- lift $ runEffect $ ((responseProducer res) >-> (toSocket csock)) >> return Nothing
       -- liftIO  $ putStrLn "sent."
       return e

hello :: (Monad m) =>
         Request
      -> Pp.Parser ByteString m (Either Pa.ParsingError (Response m))
hello req =
    let body = "hello" in
    return $ Right $ Response
               { _rsStatus  = status200
               , _rsHeaders = [("Content-Length", fromString $ show $ B.length body)
                              ]
               , _rsBody    = ResponseProducer (yield body)
               , _rsClose   = False
               }

simpleRequest :: ByteString
simpleRequest = B.concat
    [ "GET / HTTP/1.1\r\n"
    , "Host: localhost\r\n"
    , "User-Agent: curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1 zlib/1.2.3.4 libidn/1.23 librtmp/2.3\r\n"
    , "Accept: */*\r\n"
    , "\r\n"
    ]

anotherRequest :: ByteString
anotherRequest = "GET / HTTP/1.1\r\nUser-Agent: curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1 zlib/1.2.3.4 libidn/1.23 librtmp/2.3\r\nHost: localhost:8000\r\nAccept: */*\r\n\r\n"
{-
{-

We need to read a Response and write  Request.

If the Requset contains a connection-close header, then we should close the connection after sending the Response.
If the Response contains a connection-close header, then we should also close the connection after sending the Response.

If a 100-continue is in play, things are a bit more complicated.

-}
simpleTest :: IO ()
simpleTest =
    do r <- Pp.evalStateT (simpleOne stdoutResponse hello) (yield simpleRequest)
       case r of
         (Nothing, close) -> return ()
         (Just e, _) -> error  (show e)
-}
{-
fromVerboseSocket :: (MonadIO m) =>
                     Socket
                  -> Int
                  -> Producer' ByteString m ()
fromVerboseSocket socket limit =
    fromSocket socket limit >->
     (forever $
       do bs <- await
          lift $ liftIO $ print bs
          yield bs
          yield " "
     )

simpleServe :: HostPreference
            -> ServiceName
            -> (Request -> Pp.Parser ByteString IO (Either Pa.ParsingError (Response IO)))
            -> IO ()
simpleServe hp port handler =
    runSafeT $ serve hp port $ \(csock, clientAddr) ->
        Pp.evalStateT (go (csock, clientAddr)) (fromSocket csock 4096)
    where
      go (csock, clientAddr) =
        do r <- simpleOne (socketResponse csock) handler
           case r of
             (Nothing, True) ->
                 do liftIO $ close csock
                    return ()
             (Nothing, False) ->
                 do go (csock, clientAddr)
             (Just e, _) ->
                 do liftIO $ close csock
                    error (show e)

simpleServe2 :: forall m. (Functor m, Ps.MonadSafe m) =>
                HostPreference
             -> ServiceName
             -> (Request -> Pp.Parser ByteString m (Either Pa.ParsingError (Response m)))
             -> m ()
simpleServe2 hp port handler =
    listen hp port $ \(lsock, hostAddr) ->
        forever $ (accept lsock $
                    \(csock, clientAddr) ->
                        (Pp.evalStateT (go (csock, clientAddr)) (fromSocket csock 4096))) `Ps.catch`
                    ((\e  -> return ()) :: Ps.SomeException -> m ())
    where
--      go :: (MonadIO m) => (Socket, SockAddr) -> Pp.StateT (Producer ByteString (Ps.SafeT m) x) (Ps.SafeT m) ()
      go (csock, clientAddr) =
        do r <- simpleOne (socketResponse csock) handler
           case r of
             (Nothing, True) ->
                 do liftIO $ close csock
                    return ()
             (Nothing, False) ->
                 do go (csock, clientAddr)
             (Just e, _) ->
                 do liftIO $ close csock
                    error (show e)

data ServeState
    = Cont (Socket, SockAddr)
    | Close
    | Error Pa.ParsingError

{-

In this variant, we will limit the maxinum simulataneous connections.

-}
simpleServe3 :: HostPreference
             -> ServiceName
             -> (Request -> Pp.Parser ByteString IO (Either Pa.ParsingError (Response IO)))
             -> IO ()
simpleServe3 hp port handler =
    do connSem <- SSem.new 10
       runSafeT $ listen hp port $ \(lsock, hostAddr) ->
        liftIO $ forever $
                  (do SSem.wait connSem
                      print =<< getValue connSem
                      conn@(csock,_) <- N.accept lsock
                      forkFinally
                             (Pp.evalStateT (go (Cont conn)) (fromSocket csock 4096))
                             (\ea ->
                                  do NS.closeSock csock
                                     signal connSem
                                     either E.throwIO return ea
                             )

                      return  ())


{-
    listen hp port $ \(lsock, hostAddr) ->
        forever $ (accept lsock $
                    \(csock, clientAddr) ->
                        (Pp.evalStateT (go (csock, clientAddr)) (fromSocket csock 4096))) `Ps.catch`
                    ((\e  -> return ()) :: Ps.SomeException -> m ())
-}
    where
      go :: ServeState -> Pp.Parser ByteString IO ()
      go Close = return ()
      go (Cont c) = go =<< step c
      go (Error e) = error (show e)
      step (csock, clientAddr) =
        do r <- simpleOne (socketResponse csock) handler
           case r of
             (Nothing, True) ->
                 do return Close
             (Nothing, False) ->
                 do return (Cont (csock, clientAddr))
             (Just e, _) ->
                 do return (Error e)
-}
{-
{-

This variant uses pipes-concurrency and work stealing to limit the
number of requests being handled at once. In this version, we can
handle a single request from a pipelined connection and push the
remainder back into the work pool.

This prevents someone from monopolizing all the worker threads by
pipelining an unbounded number of requests.

-- NOTE: this dies with out printing anything if Pc.Bounded is too
-- small compared to the number of incoming connections.
-}
simpleServe4 :: HostPreference
             -> ServiceName
             -> (Request -> Pp.Parser ByteString IO (Either Pa.ParsingError (Response IO)))
             -> IO ()
simpleServe4 hp port handler =
    do -- create work queue
       (output, input) <- Pc.spawn (Pc.Bounded 1024)

      -- start worker threads
       as <- forM [1..16] $ \i ->
             async $ (runEffect $ Pc.fromInput input >-> worker output i) `E.finally`
                                                                             (putStrLn ("worker exited: " ++ show i) >>  performGC)

       -- bind to socket
       x@(lsock, _) <- NS.bindSock hp port
       N.listen lsock 16

       -- accept connections and add work to the queue
       runEffect $ ((liftIO $ N.accept lsock) >>= \(csock, sockAddr) -> return (InOutClose (fromSocket csock 4096) (socketResponse csock) (NS.closeSock csock))) >~
                   (Pc.toOutput output)

--       runEffect $ ((((liftIO $ N.accept lsock) `E.catch` (\e -> putStrLn "accept failed." >> E.throwIO (e :: Ps.SomeException)))  >>= \(csock, sockAddr) -> return (InOutClose (fromSocket csock 4096) (socketResponse csock) (NS.closeSock csock))) `E.catch` (\e -> print (e :: Ps.SomeException) >> E.throwIO e)
--                  >~ Pc.toOutput output) `E.finally` (putStrLn "closing listening socket." >> N.close lsock)

       return ()

    where
      worker :: Output (InOutClose Pa.ParsingError IO) -> Int -> Consumer' (InOutClose Pa.ParsingError IO) IO ()
      worker output i = forever $  (do
--                 liftIO $ putStrLn $ show (i :: Int) ++ " waiting."
                 triple <- await
--                 liftIO $ putStrLn $ show (i :: Int) ++ " processing."
                 ((mErr, done), contP) <- lift $ Pp.runStateT (simpleOne (outResponse triple) handler) (inByteString triple)
                 liftIO $ if done
                    then do closeIt triple
                    else do STM.atomically $ Pc.send output (triple { inByteString = contP })
                            threadDelay 10 -- this is just for testing to make it easier to see that the work is being handled by multiple workers for a single persistent connection.
                 return ())  `Ps.catch` (\e -> lift $ print (e :: Ps.SomeException))
-}
{-

What if we want to re-use the worker stuff from simpleServe4, but we
want to use it with SSL and then listening socket is being passed in?

-- note: this doesn't seem to handle burst connections well
-- probably because we don't do the right thing with the left overs from simpleOne or something?
p
-}
simpleServe5' :: RequestParser Pa.ParsingError IO
              -> Effect IO (InOutClose Pa.ParsingError IO)
              -> (Request -> Pp.Parser ByteString IO (Either Pa.ParsingError (Response IO)))
              -> IO ()
simpleServe5' parseRequest acceptor handler =
    do -- create work queue
       (output, input) <- Pc.spawn (Pc.Bounded 4)

      -- start worker threads
       as <- forM [1..3] $ \i ->
             async $ (runEffect $ Pc.fromInput input >-> worker output i)
                       `E.finally`
                         (putStrLn ("worker exited: " ++ show i) >>  performGC)


       -- accept connections and add work to the queue
       runEffect $ acceptor >~ (Pc.toOutput output)

--       runEffect $ ((((liftIO $ N.accept lsock) `E.catch` (\e -> putStrLn "accept failed." >> E.throwIO (e :: Ps.SomeException)))  >>= \(csock, sockAddr) -> return (InOutClose (fromSocket csock 4096) (socketResponse csock) (NS.closeSock csock))) `E.catch` (\e -> print (e :: Ps.SomeException) >> E.throwIO e)
--                  >~ Pc.toOutput output) `E.finally` (putStrLn "closing listening socket." >> N.close lsock)

       return ()

    where
      worker :: Output (InOutClose Pa.ParsingError IO) -> Int -> Consumer' (InOutClose Pa.ParsingError IO) IO ()
      worker output i = forever $  (do
--                 liftIO $ putStrLn $ show (i :: Int) ++ " waiting."
                 triple <- await
--                 liftIO $ putStrLn $ show (i :: Int) ++ " processing."
                 ((mErr, done), p) <- lift $ Pp.runStateT (simpleOne parseRequest (outResponse triple) handler) (inByteString triple)
                 liftIO $ if done
                    then do closeIt triple
                    else do STM.atomically $ Pc.send output (triple { inByteString = p })
--                            threadDelay 10 -- this is just for testing to make it easier to see that the work is being handled by multiple workers for a single persistent connection.
                            return ()
                 return ())  `Ps.catch` (\e -> lift $ print (e :: Ps.SomeException))

simpleServe5 :: RequestParser Pa.ParsingError IO
             -> HostPreference
             -> ServiceName
             -> (Request -> Pp.Parser ByteString IO (Either Pa.ParsingError (Response IO)))
             -> IO ()
simpleServe5 parseRequest hp port handler =
    do -- bind to socket
       x@(lsock, _) <- NS.bindSock hp port
       N.listen lsock 16
       let acceptor = lift $ (liftIO $ N.accept lsock) >>= \(csock, sockAddr) -> return (InOutClose (fromSocket csock 4096) (socketResponse csock) (NS.closeSock csock))
       simpleServe5' parseRequest acceptor handler
       return ()

{-
can we use our pure 'simpleRequest' ?

Note that due to the way >~ works, a copy of the triple will be passed
to each worker - so even though we only appear yield once, we will
actually yield as many times as we have workers 
?
-}
{-
simpleServe6 :: a
             -> b
             -> (Request -> Pp.Parser ByteString IO (Either Pa.ParsingError (Response IO)))
             -> IO ()
simpleServe6 _ _ handler =
    let triple = InOutClose { inByteString  = yield simpleRequest
                            , outResponse   = stdoutResponse
                            , closeIt       = return ()
                            }
    in simpleServe5' (return triple) handler
-}
{-
{-

How can we add throttling? Limit to n reqs/second?
p
What does that even mean ? Do we accept up to n connections per second and then wait? Or accept a connection only every (second/n)?

-}

simpleServe7 :: a
             -> b
             -> (Request -> Pp.Parser ByteString IO (Either Pa.ParsingError (Response IO)))
             -> IO ()
simpleServe7 _ _ handler =
    let triple = InOutClose { inByteString  = yield simpleRequest
                            , outResponse   = stdoutResponse
                            , closeIt       = return ()
                            }
    in simpleServe5' (return triple) handler


-}
serveTest :: IO ()
serveTest = simpleServe5 ppRequest HostAny "8000" hello

