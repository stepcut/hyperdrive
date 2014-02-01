{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Control.Monad.Identity
import Control.Monad.State.Strict (evalStateT, get)
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


intProducer :: (Monad m) => Producer' Int m ()
intProducer = mapM_ yield [0..]

intTest :: IO ()
intTest =
    do runEffect $ intProducer >-> P.take 5 >-> P.show >-> P.stdoutLn
       runEffect $ intProducer >-> P.take 5 >-> P.show >-> P.stdoutLn

intProducerIO ::  IO (Producer Int IO ())
intProducerIO =
    do ref <- newIORef 0
       return $ forever $
              do n <- liftIO $ readIORef ref
                 liftIO $ writeIORef ref (succ n)
                 yield n

intIOTest :: IO ()
intIOTest =
    do p <- intProducerIO
       runEffect $ p >-> P.take 5 >-> P.show >-> P.stdoutLn
       runEffect $ p >-> P.take 5 >-> P.show >-> P.stdoutLn

read5 :: Producer' Int (StateT (Producer Int IO ()) IO) ()
read5 =
    do input >-> P.take 5

read5_test1 :: IO ()
read5_test1 =
    evalStateT (do runEffect $ read5 >-> P.show >-> P.stdoutLn
                   runEffect $ read5 >-> P.show >-> P.stdoutLn)
               intProducer

read5_test2 :: IO ()
read5_test2 = do
    intP <- intProducerIO
    evalStateT (do runEffect $ read5 >-> P.show >-> P.stdoutLn
                   runEffect $ read5 >-> P.show >-> P.stdoutLn)
               intP


subProcessor :: forall a m. (Show a, Monad m, MonadIO m) =>
                (Producer a m () -> m ())
             -> Producer a (StateT (Producer a m ()) m) ()
subProcessor handler =
    do input >-> P.take 1 >-> P.show >-> P.stdoutLn
       p <- get :: (Monad m) => Producer a (StateT (Producer a m ()) m) (Producer a m ())
       let p1 :: (Monad m) => Producer a m ()
           p1 =  (p >-> P.take 1)
       lift $ lift $ handler p1
--       lift $ lift $ runEffect $ p1 >-> (await >> return ())
       input >-> P.take 1 >-> P.show >-> P.stdoutLn


subProcessor_test1 :: IO ()
subProcessor_test1 =
    evalStateT (runEffect (subProcessor (\p -> putStrLn "sub-process" >> (runEffect $ p >-> P.print) >> putStrLn "done.") >-> P.print)) intProducer

subProcessor_test2 :: IO ()
subProcessor_test2 =
    do intP <- intProducerIO
       evalStateT (runEffect (subProcessor (\p -> putStrLn "sub-process" >> (runEffect $ p >-> P.print) >> putStrLn "done.") >-> P.print)) intP

subProcessor_test3 :: IO ()
subProcessor_test3 =
    do intP <- intProducerIO
       evalStateT (runEffect (subProcessor (\p -> putStrLn "sub-process" >> putStrLn "done.") >-> P.print)) intP

subProcessor_test4 :: IO ()
subProcessor_test4 =
    do intP <- intProducerIO
       evalStateT (runEffect (subProcessor (\p -> putStrLn "sub-process" >> (runEffect $ p >-> P.print) >> putStrLn "done.") >-> P.print)) intP

subProcessor' :: forall a m. (Show a, Monad m, MonadIO m) =>
                (Producer a m () -> m ())
             -> Producer a (StateT (Producer a m ()) m) ()
subProcessor' handler =
    do input >-> P.take 1 >-> P.show >-> P.stdoutLn
       p <- get :: (Monad m) => Producer a (StateT (Producer a m ()) m) (Producer a m ())
       let p1 :: (Monad m) => Producer a m ()
           p1 =  (p >-> P.take 1)
       lift $ lift $ handler p1
--       lift $ lift $ runEffect $ p1 >-> (await >> return ())
       input >-> P.take 1 >-> P.show >-> P.stdoutLn


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
         (Right (l, _)) ->
             do p <- get
                lift $ handler (p >-> P.take 1)
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

serve_test1 = serve (yield testStream) printBody

