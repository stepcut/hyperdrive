{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.IORef             (IORef(..), newIORef, readIORef, writeIORef)
import           Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as Pb

-- | this a pure producer
pureBS :: (Monad m) => Producer ByteString m ()
pureBS = yield "abcdefghijklmnopqrstuvwxyz"

-- | if we run the producer twice it will produce the same results --
-- starting from 1 each time.
pureBS_test :: IO ()
pureBS_test =
    do runEffect $ over (Pb.take 5) pureBS
{-
    do runEffect $ pureBS >-> P.take 5 >-> P.print
       putStrLn "<<Intermission>>"
       runEffect $ pureBS >-> P.take 5 >-> P.print
-}

