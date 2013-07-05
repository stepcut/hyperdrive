{-# LANGUAGE OverloadedStrings #-}
module Main where

import Pipes
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import Serve                           (serve)
import Types                           (Handler, Response(..))

------------------------------------------------------------------------------
-- pong handler
------------------------------------------------------------------------------

pong :: Handler IO
pong () =
    do req <- request ()
       let body = "PONG"
           res = Response { rsCode    = 200
                          , rsHeaders = [("Content-Length", C.pack (show (B.length body)))]
--                          , rsBody    = respond body
                          }
       respond res
       return ()

------------------------------------------------------------------------------
-- main
------------------------------------------------------------------------------

main :: IO ()
main = serve "8000" pong
