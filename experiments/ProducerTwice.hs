module Main where

import Pipes
import qualified Pipes.Prelude as P
import Pipes.Network.TCP
-- import Network.Simple.TCP (listen, accept)


main :: IO ()
main =
    listen HostAny "8000" $ \(lsock, _) ->
       accept lsock $ \(sock, sockAddr) ->
           do let p = fromSocket sock 10
              putStrLn "running first time."
              runEffect $ p >-> P.take 2 >-> P.print
              putStrLn "running second time."
              runEffect $ p >-> P.take 2 >-> P.print

