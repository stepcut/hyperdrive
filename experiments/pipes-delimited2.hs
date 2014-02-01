{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Control.Monad.Identity
import Control.Monad.State.Strict (evalStateT, get, put)
import Control.Monad.Trans
import           Data.Attoparsec.ByteString.Char8 (Parser, string)
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as B
import qualified Data.ByteString.Char8 as C
import Data.IORef
import Data.ByteString.Lex.Integral (readDecimal)
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Parse
import Pipes.Attoparsec
import qualified Pipes.ByteString as PB
import Debug.Trace (trace)

pContentLength :: Parser Int
pContentLength =
    do A.string "Content-Length: "
       lenStr <- A.takeWhile (/= '\r')
       A.string "\r\n"
       case readDecimal lenStr of
         Nothing -> fail "could not parse length."
         Just (i, bs) -> return i


serve' :: forall m. (Monad m) =>
         (Producer ByteString m () -> m ())
      -> StateT (Producer ByteString m ()) m ()
serve' handler =
    do e <- parse pContentLength
       case e of
         (Left e) -> error (show e)
         (Right (_, len)) ->
             do p <- get
                lift $ handler (p >-> PB.take len)
                put (p >-> PB.drop len) -- needed if the producer is pure -- FIXME
                parse "\r\n"
                b <- isEndOfParserInput
                when (not b) $ serve' handler

serve :: Monad m =>
         Producer ByteString m ()
     -> (Producer ByteString m () -> m ())
     -> m ()
serve p h = evalStateT (serve' h) p


testStream = "Content-Length: 5\r\n12345\r\n"

printBody :: Producer ByteString IO () -> IO ()
printBody p = runEffect $ p >-> P.print

serve_test1 = serve (yield testStream >> yield testStream) printBody
