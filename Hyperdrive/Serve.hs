{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module Hyperdrive.Serve where

import Control.Applicative        ((<*))
import Data.ByteString            (ByteString)
import Data.CaseInsensitive       (mk)
import Hyperdrive.Types           (Request(..), Response(..), RequestBodyLength(KnownLength))
import Lens.Family.State.Strict   (zoom)
import qualified Pipes.ByteString as Pb
import qualified Pipes.Parse      as Pp

hasConnectionClose :: Request -> Bool
hasConnectionClose req =
    case lookup "connection" (_rqHeaders req) of
      Nothing  -> False
      (Just _) -> True

-- | parse a single 'Request' including the 'Request' body
parseOne :: (Monad m, Functor m) =>
         Pp.Parser ByteString m (Either e Request)
      -> (Request -> Pp.Parser ByteString m (Either e r))
      -> Pp.Parser ByteString m (Either e r, Bool)
parseOne pRequest handler =
    do e <- pRequest
       case e of
         (Left e) -> return (Left e, True)
         (Right req) ->
             do let (KnownLength len) = _rqBodyLength req
                e <- zoom (Pb.splitAt len) ((handler req) <* Pp.skipAll)
                return (e, hasConnectionClose req)

addConnectionClose :: Bool -> Response m -> Response m
addConnectionClose False res = res
addConnectionClose True res  = res { _rsHeaders = ("Connection", "close") : (_rsHeaders res) }

-- | parse a single 'Request' and send a single 'Response'
--
-- this should leave the parser in such a state that additional
-- requests can be parsed
handleOne :: (Functor m, Monad m) =>
             Pp.Parser ByteString m (Either e Request)
          -> (Response m -> Pp.Parser ByteString m (Maybe e))
          -> (Request -> Pp.Parser ByteString m (Either e (Response m)))
          -> Pp.Parser ByteString m (Maybe e, Bool)
handleOne pRequest sendResponse handler =
    do (e, close) <- parseOne pRequest handler
       case e of
         (Left e)  -> return (Just e, close)
         (Right r) ->
             do e <- sendResponse (addConnectionClose (close || _rsClose r) r)
                return (e, close || _rsClose r)
