module Main (main) where

import ConnectionTests (connectionTests)
import CorpusTests (corpusTests)
import DecodeTests (decodeTests)
import EncodeTests (encodeTests)
import FrameTests (frameTests)
import HuffmanTests (huffmanTests)
import IntegerTests (integerTests)
import ScheduleTests (scheduleTests)
import StateTests (stateTests)
import StreamTests (endingTests, priorityTests, responseTests, streamTests)
import RFC7541 (huffmanTable, rfc7541)
import Test.Tasty

main :: IO ()
main = do
  text <- rfc7541
  corpus <- corpusTests
  defaultMain $ localOption (mkTimeout (120 * 1000000)) $ testGroup "hyperdrive-http2"
    [ integerTests, huffmanTests (huffmanTable text), decodeTests text, encodeTests, corpus, frameTests, connectionTests, streamTests, responseTests, endingTests, stateTests, scheduleTests, priorityTests ]
