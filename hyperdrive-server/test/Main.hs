module Main (main) where

import ServerTests (serverTests)
import Test.Tasty

main :: IO ()
main = defaultMain $ localOption (mkTimeout (120 * 1000000)) serverTests
