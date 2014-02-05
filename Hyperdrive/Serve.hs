{-# LANGUAGE RankNTypes #-}
module Hyperdrive.Serve where

import Control.Applicative        ((<*))
import Data.ByteString            (ByteString)
import Hyperdrive.Types           (Request(..), Response(..), RequestBodyLength(KnownLength))
import Lens.Family.State.Strict   (zoom)
import qualified Pipes.ByteString as Pb
import qualified Pipes.Parse      as Pp

data HyperdriveError
    = HyperdriveError

-- | handle a single 'Request'
serveOne :: (Monad m, Functor m) =>
         Pp.Parser ByteString m (Either e Request)
      -> (Request -> Pp.Parser ByteString m (Either e r))
      -> Pp.Parser ByteString m (Either e r)
serveOne pRequest handler =
    do e <- pRequest
       case e of
         (Left e) -> return (Left e)
         (Right req) ->
             do let (KnownLength len) = _rqBodyLength req
                e <- zoom (Pb.splitAt len) ((handler req) <* Pp.skipAll)
                return e

-- sendResponse

