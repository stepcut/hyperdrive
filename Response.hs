{-# LANGUAGE RankNTypes, RecordWildCards, OverloadedStrings #-}
module Response where

import Pipes
import Data.ByteString           (ByteString)
import qualified Data.ByteString as B
import Types                     (Response(..))

-- TODO: How do output the data in sizes that are network friendly? Should we leverage blaze builder?

------------------------------------------------------------------------------
-- responseWriter
------------------------------------------------------------------------------
{-
responseWriter :: Monad m =>
                  Response m
               -> Pipe ByteString ByteString m ()
-}

-- responseWriter :: () -> Pipe Response ByteString IO ()
responseWriter () =
    do Response{..} <- request ()
       respond $ B.concat [ statusLine rsCode
                          , renderHeaders rsHeaders
                          , "\r\n"
                          ]
--       rsBody

------------------------------------------------------------------------------
-- Status Lines
------------------------------------------------------------------------------

{-
  Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF
-}

-- FIXME: can the http version always be 1.1 or do we need to match the caller?
statusLine :: Int -> ByteString
statusLine 200 = ok_status
statusLine 404 = not_found_status

ok_status :: ByteString
ok_status = "HTTP/1.1 200 OK\r\n"

not_found_status :: ByteString
not_found_status = "HTTP/1.1 404 Not Found\r\n"

------------------------------------------------------------------------------
-- Headers
------------------------------------------------------------------------------

renderHeaders :: [(ByteString, ByteString)] -> ByteString
renderHeaders = B.concat . map renderHeader

renderHeader :: (ByteString, ByteString) -> ByteString
renderHeader (fieldName, fieldValue) =
    B.concat [fieldName, ": ", fieldValue, "\r\n"]
