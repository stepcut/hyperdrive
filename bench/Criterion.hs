{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import Data.Attoparsec
import Network.Socket (SockAddr(SockAddrUnix))
import Request

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "parse"
      [ bench "pRequest" $ whnf (parseOnly (pRequest False (SockAddrUnix "dummy-sockaddr"))) "GET /foo HTTP/1.1\r\n"
      ]
    ]

main :: IO ()
main = defaultMain benchmarks

