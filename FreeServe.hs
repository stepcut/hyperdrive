{-# LANGUAGE DeriveFunctor, RecordWildCards, OverloadedStrings, RankNTypes #-}
module FreeServe where

import Control.Monad
import Control.Monad.Trans.Free (FreeT(..),FreeF(Free, Pure))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Pipes
import Pipes.Network.TCP
import qualified Pipes.Parse as P
import Response (statusLine, renderHeaders)
import Request (parseRequest)
import Types
import Network (Socket)
import Network.Socket.ByteString (sendAll)

-- Each request is an effectful stream of `ByteString` chunks associated with a
-- request header
data RequestF next = RequestF
    { requestHead   :: Request
    , requestBody   :: Producer ByteString IO next
    } deriving (Functor)
-- Note that the `next` type parameter indicats where the next request will be located.
-- In this case we require that the next request is located inside the return value of
-- the producer in order to enforce that we can't begin reading the next request until
-- we first drain this request in its entirety.

-- Each response is also a header and effectful stream of `ByteString` chunks
data ResponseF next = ResponseF
    { responseHead :: Response
    , responseBody :: Producer ByteString IO next
    } deriving (Functor)

-- A stream of requests uses `FreeT` to delimit each request.
-- This preserves the original chunking and laziness while still
-- allowing you to logically group the chunks into separate requests.
-- `FreeT` ensures that each subsequent request is embedded within
-- the `next` field of the `Request` type.  The reason it knows how to
-- do this is because of the `deriving (Functor)` stuff.  `FreeT` uses the
-- `Functor` instance of the `Request` type to detect where to insert the
-- next request.
type Requests = FreeT RequestF IO ()

-- The stream of responses also uses `FreeT` to delimit each response
type Responses = FreeT ResponseF IO ()

writeResponses :: Socket -> Responses -> IO ()
writeResponses socket responses =
    do x <- runFreeT responses
       case x of
         (Pure ()) ->  return ()
         (Free (ResponseF Response{..} body)) ->
             do sendAll socket $ B.concat [ statusLine rsCode
                                          , renderHeaders rsHeaders
                                          , "\r\n"
                                          ]
                res <- runEffect $ body >-> (toSocket socket)
                writeResponses socket res

readRequests :: Socket -> SockAddr -> Requests
readRequests socket clientAddr =
    do (r, p) <- P.runStateT (parseRequest False clientAddr) (fromSocket socket 4096)
       case r of
         (Left parseError) -> error $ show parseError
         (Right (bytesRead, request)) ->
             let requestBody :: Producer ByteString IO Requests
                 requestBody =
                     do readRequestBody request
                        return (readRequests socket clientAddr)
             in FreeT $ return (Free (RequestF request requestBody))

-- | for POST and PUT requests this should actually parse the message body and 'yield' the contents
readRequestBody :: Request -> Producer ByteString IO ()
readRequestBody _ =
    do return ()

server :: (forall r. Request -> IO (Response, Pipe ByteString ByteString IO ()))
         -> (Requests -> Responses)
server f requests = FreeT $ do
  x <- runFreeT requests
  case x of
    Pure r -> return (Pure r)
    Free (RequestF request producer) ->
        do (header, p) <- f request
           let newProducer = do
                 remainingRequests <- producer >-> (p >> forever await)
                 return (server f remainingRequests)
           return (Free (ResponseF header newProducer))

pong :: Request -> IO (Response, Pipe ByteString ByteString IO ())
pong req =
    do let body = "PONG"
           res = Response { rsCode    = 200
                          , rsHeaders = [("Content-Length", C.pack (show (B.length body)))]
                          }
       return (res, yield body)

main :: IO ()
main =
    let port = "8000" in
    listen (Host "127.0.0.1") port $ \(listenSocket, listenAddr) ->
    forever $
      acceptFork listenSocket $ \(acceptedSocket, clientAddr) ->
          writeResponses acceptedSocket ((server pong) (readRequests acceptedSocket clientAddr))
