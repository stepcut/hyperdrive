{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module Hyperdrive.Serve where

import Control.Applicative        ((<*))
import Data.ByteString            (ByteString)
import Data.CaseInsensitive       (mk)
import Data.Monoid                (mempty)
import Hyperdrive.Types           (Request(..), Response(..), RequestBodyLength(KnownLength))
import Lens.Family.State.Strict   (zoom)
import qualified Pipes.ByteString as Pb
import qualified Pipes.Parse      as Pp

type RequestParser e m = Pp.Parser ByteString m (Maybe (Either e Request))

hasConnectionClose :: Request -> Bool
hasConnectionClose req =
    case lookup "connection" (_rqHeaders req) of
      Nothing  -> False
      (Just _) -> True

-- | parse a single 'Request' including the 'Request' body
parseOne :: (Monad m, Functor m) =>
         RequestParser e m -- Pp.Parser ByteString m (Maybe (Either e Request))
      -> (Request -> Pp.Parser ByteString m (Either e r))
      -> Pp.Parser ByteString m (Maybe (Either e r), Bool)
parseOne pRequest handler =
    do me <- pRequest
       case me of
         Nothing -> return (Nothing, True)
         (Just (Left e)) -> return (Just $ Left e, True)
         (Just (Right req)) ->
             do let (KnownLength len) = _rqBodyLength req
                me' <- zoom (Pb.splitAt len) ((handler req) <* Pp.skipAll)
                return (Just $ me', hasConnectionClose req)


addConnectionClose :: Bool -> Response m -> Response m
addConnectionClose False res = res
addConnectionClose True res  = res { _rsHeaders = ("Connection", "close") : (_rsHeaders res) }

-- | parse a single 'Request' and send a single 'Response'
--
-- this should leave the parser in such a state that additional
-- requests can be parsed
handleOne :: (Functor m, Monad m) =>
             RequestParser e m
          -> (Response m -> Pp.Parser ByteString m (Maybe e))
          -> (Request -> Pp.Parser ByteString m (Either e (Response m)))
          -> Pp.Parser ByteString m (Maybe e, Bool)
handleOne pRequest sendResponse handler =
    do (e, close) <- parseOne pRequest handler
       case e of
         Nothing -> return (Nothing, close)
         (Just (Left e))  -> return (Just e, close)
         (Just (Right r)) ->
             do e <- sendResponse (addConnectionClose (close || _rsClose r) r)
                return (e, close || _rsClose r)
