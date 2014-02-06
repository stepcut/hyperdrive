{-# LANGUAGE RankNTypes #-}
module Hyperdrive.Serve where

import Control.Applicative        ((<*))
import Data.ByteString            (ByteString)
import Hyperdrive.Types           (Request(..), Response(..), RequestBodyLength(KnownLength))
import Lens.Family.State.Strict   (zoom)
import qualified Pipes.ByteString as Pb
import qualified Pipes.Parse      as Pp

-- | parse a single 'Request' including the 'Request' body
parseOne :: (Monad m, Functor m) =>
         Pp.Parser ByteString m (Either e Request)
      -> (Request -> Pp.Parser ByteString m (Either e r))
      -> Pp.Parser ByteString m (Either e r)
parseOne pRequest handler =
    do e <- pRequest
       case e of
         (Left e) -> return (Left e)
         (Right req) ->
             do let (KnownLength len) = _rqBodyLength req
                e <- zoom (Pb.splitAt len) ((handler req) <* Pp.skipAll)
                return e

-- | parse a single 'Request' and send a single 'Response'
--
-- this should leave the parser in such a state that additional
-- requests can be parsed
handleOne :: (Functor m, Monad m) =>
             Pp.Parser ByteString m (Either e Request)
          -> (Response m -> Pp.Parser ByteString m (Maybe e))
          -> (Request -> Pp.Parser ByteString m (Either e (Response m)))
          -> Pp.Parser ByteString m (Maybe e)
handleOne pRequest sendResponse handler =
    do e <- parseOne pRequest handler
       case e of
         (Left e)  -> return (Just e)
         (Right r) -> sendResponse r

{-
s :: (Monad m, Functor m) =>
         Pp.Parser ByteString m (Either e Request)
      -> (Request -> Pp.Parser ByteString m (Either e r))
      -> Pp.Parser ByteString m (Either e r)
se pRequest sResponse handler = go
    where
      go =
          do e <- parseOne pRequest handler
             case e of
               (Left e)  -> return e
               (Right r) ->
                   do -- sResponse r
                      og
-}