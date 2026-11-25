module Main (main) where

import HuffmanTests (huffmanTests)
import IntegerTests (integerTests)
import RFC7541 (huffmanTable, rfc7541)
import Test.Tasty

main :: IO ()
main = do
  text <- rfc7541
  defaultMain $ localOption (mkTimeout (120 * 1000000)) $ testGroup "hyperdrive-http2"
    [ integerTests, huffmanTests (huffmanTable text) ]
