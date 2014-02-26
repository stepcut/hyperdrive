{-# LANGUAGE OverloadedStrings, RankNTypes, RecordWildCards, OverloadedStrings #-}
{-

Simple implementations for parsing Requests and generating Responses.

Right now there is no attempt to make them correct. Mostly they are
just dummy implementations so that we can work out the rest of the API
and then later drop in a solid parser.

-}
module Hyperdrive.Simple where

import Control.Applicative ((*>), pure, many)
import Control.Monad             (forever)
import Control.Monad.Trans       (MonadIO(liftIO))
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.CaseInsensitive (mk, original)
import Data.String (fromString)
import Hyperdrive.Serve (handleOne)
import Hyperdrive.Types (Request(..), RequestBodyLength(..), Response(..), ResponseBody(..))
import Network.HTTP.Types        (HeaderName, ResponseHeaders, Status(..), HttpVersion(..), status200)
import Network.Socket            (Socket, SockAddr(..), close)
import Network.Simple.TCP        (HostPreference(..), ServiceName(..))
import Pipes                     (Producer, Producer', (>->), await, runEffect, lift, yield)
import qualified Pipes.Prelude   as P
import qualified Pipes.Attoparsec as Pa
import qualified Pipes.ByteString as Pb
import qualified Pipes.Parse     as Pp
import Pipes.Network.TCP.Safe (serve, toSocket,fromSocket)
import Pipes.Safe (runSafeT)

------------------------------------------------------------------------------
-- Request
------------------------------------------------------------------------------

seperators :: [Char]
seperators =
    [ '(' , ')' , '<' , '>'  , '@'
    , ',' , ';' , ':' , '\\' , '"'
    , '/' , '[' , ']' , '?'  , '='
    , '{' , '}' , ' ' , '\t'
    ]

ctls :: [Char]
ctls = '\DEL' : ['\0' .. '\31']

pToken :: A.Parser ByteString
pToken = AC.takeWhile1 (\c -> notElem c (seperators ++ ctls))

 -- a very incorrect header parser
pHeader :: A.Parser (HeaderName, ByteString)
pHeader =
    do n <- pToken
       AC.char ':'
       AC.skipWhile (== ' ')
       v <- AC.takeTill (== '\r')
       AC.take 1
       AC.char '\n'
       return (mk n, v)

pRequest :: Bool
         -> A.Parser Request
pRequest secure =
    do method      <- AC.takeWhile1 (/= ' ')
       AB.take 1
       pathInfo    <- AC.takeWhile1 (/= ' ')
       AB.take 1
       httpVersion <- AB.string "HTTP/1.1\r\n" *> pure (HttpVersion 1 1)
       headers <- many pHeader
       AC.string "\r\n"
       let req = Request
                  { _rqSecure = secure
                  , _rqMethod = method
                  , _rqRawPathInfo = pathInfo
                  , _rqRawQueryString = ""
                  , _rqVersion = httpVersion
                  , _rqHeaders = headers
                  , _rqPeer = (SockAddrUnix "")
                  , _rqPathInfo = []
                  , _rqQuery = []
                  , _rqCookies = []
                  , _rqBodyLength = KnownLength 0
                  , _rqHeaderHost = ""
                  , _rqHeaderRange = Nothing
                  }
       return req

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

ppRequest :: (Monad m) => Pp.Parser ByteString m (Either Pa.ParsingError Request)
ppRequest = Pa.parse (pRequest False)

simpleOne :: (Functor m, MonadIO m) =>
             (Response m -> Pp.Parser ByteString m (Maybe Pa.ParsingError))
          -> (Request -> Pp.Parser ByteString m (Either Pa.ParsingError (Response m)))
          -> Pp.Parser ByteString m (Maybe Pa.ParsingError, Bool)
simpleOne sendResponse handler =
    handleOne ppRequest (\res -> sendResponse res) handler

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

hello' :: (MonadIO m) =>
         Request
      -> Pp.Parser ByteString m (Either Pa.ParsingError (Response m))
hello' req = do
    lift $ liftIO $ print "hello'"
    let body = "hello"
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

socketResponse :: (MonadIO m) =>
                  Socket
               -> Response m
               -> Pp.Parser ByteString m (Maybe e)
socketResponse csock res =
    do -- liftIO $ putStrLn "sending response..."
       e <- lift $ runEffect $ ((responseProducer res) >-> (toSocket csock)) >> return Nothing
       -- liftIO  $ putStrLn "sent."
       return e

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

{-

-}
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
{-
simpleServePersistent :: HostPreference
            -> ServiceName
            -> (Request -> Pp.Parser ByteString IO (Either Pa.ParsingError (Response IO)))
            -> IO ()
simpleServePersistent hp port handler =
    runSafeT $ serve hp port $ \(csock, clienAddr) ->
        do r <- Pp.evalStateT (simpleOne (socketResponse csock) handler) (fromSocket csock 4096)
           case r of
             Nothing  ->
             (Just e) -> error (show e)


-}
serveTest = simpleServe HostAny "8000" hello