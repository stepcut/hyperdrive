module Main (main) where

import H2Tests (h2Tests)
import ServerTests (serverTests)
import Test.Tasty

-- The HTTP/2 tests move megabytes; they run after the others, some of
-- which measure time or the live heap.
main :: IO ()
main = defaultMain $ localOption (mkTimeout (120 * 1000000)) $ testGroup "server"
  [ serverTests, after AllFinish "$2 == \"hyperdrive-server\"" h2Tests ]
