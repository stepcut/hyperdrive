module Main (main) where

import ChunkedTests (chunkedTests)
import FramingTests (framingTests)
import HeadTests (headTests)
import TargetTests (targetTests)
import Test.Tasty

main :: IO ()
main = defaultMain $ localOption (mkTimeout (120 * 1000000)) $ testGroup "hyperdrive-http1"
  [ framingTests, headTests, chunkedTests, targetTests ]
