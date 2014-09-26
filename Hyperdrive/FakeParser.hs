{-# LANGUAGE OverloadedStrings #-}
module Hyperdrive.FakeParser where

import Control.Applicative        ((*>), pure, many)
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Data.ByteString            (ByteString)
import Data.CaseInsensitive       (mk)
import Hyperdrive.Types           ( Request(..), RequestBodyLength(..)
                                  , Response(..), ResponseBody(..)
                                  )
import Network.HTTP.Types         ( HeaderName, ResponseHeaders, Status(..)
                                  , HttpVersion(..), status200
                                  )
import Network.Socket             (Socket, SockAddr(..))


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

pToken :: AC.Parser ByteString
pToken = AC.takeWhile1 (\c -> notElem c (seperators ++ ctls))

 -- a very incorrect header parser
pHeader :: AC.Parser (HeaderName, ByteString)
pHeader =
    do n <- pToken
       AC.char ':'
       AC.skipWhile (== ' ')
       v <- AC.takeTill (== '\r')
       AC.take 1
       AC.char '\n'
       return (mk n, v)

fakeParseRequest :: Bool
         -> AC.Parser Request
fakeParseRequest secure =
    do method      <- AC.takeWhile1 (/= ' ')
       AC.take 1
       pathInfo    <- AC.takeWhile1 (/= ' ')
       AC.take 1
       httpVersion <- AC.string "HTTP/1.1\r\n" *> pure (HttpVersion 1 1)
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
