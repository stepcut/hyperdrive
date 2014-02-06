{-# LANGUAGE OverloadedStrings, RankNTypes, RecordWildCards, OverloadedStrings #-}
{-

Simple implementations for parsing Requests and generating Responses.

Right now there is no attempt to make them correct. Mostly they are
just dummy implementations so that we can work out the rest of the API
and then later drop in a solid parser.

-}
module Hyperdrive.Simple where

import Control.Applicative ((*>), pure, many)
import Control.Monad.Trans       (MonadIO)
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
import Network.Socket            (SockAddr(..))
import Pipes                     (Producer, (>->), runEffect, lift, yield)
import qualified Pipes.Prelude   as P
import qualified Pipes.Attoparsec as Pa
import qualified Pipes.ByteString as Pb
import qualified Pipes.Parse     as Pp

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

pToken :: A.Parser ByteString
pToken = AC.takeWhile1 (\c -> notElem c seperators)

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
          -> Pp.Parser ByteString m (Maybe Pa.ParsingError)
simpleOne sendResponse handler =
    handleOne ppRequest sendResponse handler


hello :: (Monad m) =>
         Request
      -> Pp.Parser ByteString m (Either Pa.ParsingError (Response m))
hello req =
    let body = "hello" in
    return $ Right $ Response
               { _rsStatus  = status200
               , _rsHeaders = [("Content-Length", fromString $ show $ B.length body)]
               , _rsBody    = ResponseProducer (yield body)
               }

simpleRequest :: ByteString
simpleRequest = B.concat
    [ "GET / HTTP/1.1\r\n"
    , "Host: localhost\r\n"
    , "\r\n"
    ]


simpleTest :: IO ()
simpleTest =
    do r <- Pp.evalStateT (simpleOne stdoutResponse hello) (yield simpleRequest)
       case r of
         Nothing -> return ()
         (Just e) -> error (show e)
