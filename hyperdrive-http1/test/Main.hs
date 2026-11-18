module Main (main) where

import FramingTests (framingTests)
import Test.Tasty

main :: IO ()
main = defaultMain $ localOption (mkTimeout (120 * 1000000)) $ testGroup "hyperdrive-http1"
  [ framingTests ]
