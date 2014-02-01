{-# LANGUAGE ScopedTypeVariables, RankNTypes, OverloadedStrings #-}
module Main where

import Control.Applicative
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

data RequestF m next = RequestF
    { reqLength :: !Int
    , reqBody   :: !(Producer ByteString m next)
    }

instance (Monad m) => Functor (RequestF m) where
    fmap f (RequestF len bdy) = RequestF len (fmap f bdy)

data ResponseF m next = ResponseF
    { resLength :: !Int
    , resBody   :: !(Producer ByteString m next)
    }

instance (Monad m) => Functor (ResponseF m) where
    fmap f (ResponseF l bdy) = ResponseF l (fmap f bdy)


data Response m = Response
    { rLength :: !Int
    , rBody   :: !(Producer ByteString m ())
    }


parseRequests :: forall m r. Monad m =>
                 Producer ByteString m r
              -> FreeT (RequestF m) m ()
parseRequests p0 = FreeT $ do
    (m1, p1) <- runStateT pContentLength p0
    return $ case m1 of
               (Just (Right len)) ->
                   let bdy :: Producer ByteString m (FreeT (RequestF m) m ())
                       bdy = parseRequests <$> splitBody len p1

                       req :: RequestF m (FreeT (RequestF m) m ())
                       req = RequestF len bdy
                   in Free req
               (Just (Left e)) -> error e
               Nothing -> Pure ()

splitBody :: (Monad m) =>
             Int
          -> Producer  ByteString m r
          -> Producer' ByteString m (Producer ByteString m r)
splitBody len p0 =
    do p1 <- PB.splitAt len p0
       return (p1 >-> PB.drop 2)

pContentLength' :: Parser Int
pContentLength' =
    do A.string "Content-Length: "
       lenStr <- A.takeWhile (/= '\r')
       A.string "\r\n"
       case readDecimal lenStr of
         Nothing -> fail "could not parse length."
         Just (i, bs) -> return i

pContentLength :: (Monad m) => StateT (Producer ByteString m r) m (Maybe (Either String Int))
pContentLength = do
  eof <- PB.isEndOfBytes
  if eof
     then return Nothing
     else do
       r <- parse pContentLength'
       return $ case r of
         (Left e)       -> Just (Left $ show e)
         (Right (_, i)) -> Just (Right i)

serve' :: (Monad m) =>
          (RequestF m (FreeT (RequestF m) m a) -> m (Response m, FreeT (RequestF m) m a))
       -> FreeT (RequestF m) m a
       -> FreeT (ResponseF m) m a
serve' handler requests = FreeT $
    do x <- runFreeT requests
       case x of
         Free req ->
             do (Response l p, remaining) <- handler req
                let next = serve' handler remaining
                return $ Free (ResponseF l (p >> return next))
         Pure r -> return $ Pure r

writeResponses :: Show a => FreeT (ResponseF IO) IO a -> IO ()
writeResponses resps =
    do x <- runFreeT resps
       case x of
         Pure r -> print r
         (Free (ResponseF len bdy)) ->
             do putStrLn "Response {"
                putStr "   reqLength = "
                print len
                putStr " , reqBody = "
                next <- runEffect $ bdy >-> P.print
                putStrLn "}\n"
                writeResponses next

hello :: (MonadIO m) => RequestF m (FreeT (RequestF m) m a) -> m (Response m, (FreeT (RequestF m) m a))
hello (RequestF len inP) =
    do remainingRequests <- runEffect $ inP >-> P.print
       let outputP = yield "hello"
       return (Response 5 outputP, remainingRequests)

testStream :: ByteString
testStream = "Content-Length: 5\r\n12345\r\n"

main :: IO ()
main = writeResponses (serve' hello (parseRequests (yield testStream >> yield testStream)))
