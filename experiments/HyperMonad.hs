{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, RankNTypes #-}
module Main where

import Control.Applicative              ((<$>))
import Control.Monad.Trans.State.Strict (StateT(..))
import Control.Monad                    (when)
import Data.ByteString                  (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B
import Data.ByteString.Lex.Integral     (readDecimal)
import           Data.Attoparsec.ByteString.Char8 (Parser, string, char)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Maybe                       (isNothing)
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Attoparsec
import qualified Pipes.ByteString as Pb
import Lens.Family.State.Strict (zoom)
import qualified Pipes.Parse as Ppi

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

data Request = Request
    { rqLength :: Int
    }
    deriving Show

-- | dummy serialized 'Request'
request :: ByteString
request =
    B.concat
          [ "Content-Length: 4\n"
          , "\n"
          , "1234\n"
          ]

data Response m = Response
    { rsLength :: Int
    , rsBody   :: Producer ByteString m ()
    }


type Hyperdrive m r = Ppi.Parser ByteString m r

------------------------------------------------------------------------------
-- request parser
------------------------------------------------------------------------------

pRequestHead :: A.Parser Request
pRequestHead =
    do string "Content-Length: "
       len <- notNL
       string "\n\n"
       case readDecimal len of
         Just (i,_) -> return $ Request { rqLength = i }
         _     -> error $ "Could not parse length: " ++ show len
    where
      notNL :: Parser ByteString
      notNL = A.takeWhile (/= '\n')

------------------------------------------------------------------------------
-- parsing loop
------------------------------------------------------------------------------

parseLoop :: (Monad m) => (a -> m ()) -> (Request -> Hyperdrive m a) -> Ppi.Parser ByteString m ()
parseLoop sendResponse handler =
    do eReq <- parse pRequestHead
       case eReq of
         (Left e) -> error (show e)
         (Right req) ->
             do a <- zoom (Pb.splitAt (rqLength req)) (handler req)
                lift $ sendResponse a
                r <- parse (char '\n' >> (isNothing <$> A.peekChar))
                case r of
                  (Right False) -> parseLoop sendResponse handler
                  (Right True)  -> return ()
                  (Left e)      -> error (show e)

------------------------------------------------------------------------------
-- parse body
------------------------------------------------------------------------------

-- | get the whole request body
pBody :: (Monad m) => Hyperdrive m (Either ParsingError ByteString)
pBody = parse A.takeByteString


------------------------------------------------------------------------------
-- 'Response IO' helpers
------------------------------------------------------------------------------

sendResponse :: Response IO
             -> IO ()
sendResponse res =
    runEffect $ (rsBody res) >-> P.print


serve :: (Request -> Hyperdrive IO (Response IO))
      -> Producer ByteString IO ()
      -> IO ()
serve h p = Ppi.evalStateT (parseLoop sendResponse h) p

------------------------------------------------------------------------------
-- simple test
------------------------------------------------------------------------------

-- | echo the entire body
--
-- Note: we consume the entire body before sending the 'Response'. Not
-- a stellar example of streaming IO ;)
echoBody :: (Monad m) => Request -> Hyperdrive m (Response m)
echoBody req =
    do bd <- pBody
       case bd of
         (Left e) -> error "Malformed Request"
         (Right bd) ->
             return $ Response { rsLength = (rqLength req)
                               , rsBody   = yield bd
                               }

-- | simple serving example
test :: IO ()
test = serve echoBody (yield request >> yield request)
