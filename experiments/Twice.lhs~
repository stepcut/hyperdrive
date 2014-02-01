module Main where

import Data.IORef             (IORef(..), newIORef, readIORef, writeIORef)
import           Pipes
import qualified Pipes.Prelude as P

-- | this a pure producer
pure10 :: (Monad m) => Producer Int m ()
pure10 = mapM_ yield [1..10]

-- | if we run the producer twice it will produce the same results --
-- starting from 1 each time.
pure10_test :: IO ()
pure10_test =
    do runEffect $ pure10 >-> P.take 5 >-> P.print
       putStrLn "<<Intermission>>"
       runEffect $ pure10 >-> P.take 5 >-> P.print

ioify :: Producer Int IO () -> IO (Producer Int IO ())
ioify p0 =
    do ref <- liftIO $ newIORef p0
       return (go ref)
    where
      go :: IORef (Producer Int IO ()) -> Producer Int IO ()
      go ref =
          do p <- liftIO $ readIORef ref
             x <- liftIO $ next p
             case x of
               (Right (i, p')) ->
                   do liftIO $ writeIORef ref p'
                      yield i
                      go ref
               (Left ()) ->
                   do liftIO $ writeIORef ref (return ())
                      return ()

impure10_test :: IO ()
impure10_test =
    do p <- ioify pure10
       runEffect $ p >-> P.take 5 >-> P.print
       putStrLn "<<Intermission>>"
       runEffect $ p >-> P.take 5 >-> P.print
