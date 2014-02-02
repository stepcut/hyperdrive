module Main where

import           Pipes
import qualified Pipes.Prelude as P
import qualified Network.Socket as NS
import qualified Pipes.Network.TCP       as T
import qualified Pipes.Network.TCP.Safe  as T'

host1  = "127.0.0.1"                        :: NS.HostName
host1p = T.Host host1                       :: T.HostPreference

main :: IO ()
main =
    NS.withSocketsDo $
      T.listen host1p "8000" $ \(lsock, _laddr) ->
          T.accept lsock $ \(csock, _caddr) ->
              runEffect $ (T.socketReadS 4096 csock >-> P.print) ()

-- T.socketReadS 4096 csock

-- printD () =
--        return ()
