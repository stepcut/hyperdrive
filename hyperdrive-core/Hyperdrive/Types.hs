{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, RankNTypes, StandaloneDeriving, TemplateHaskell, OverloadedStrings #-}
module Hyperdrive.Types where

import Pipes                 (Producer)
import Data.ByteString       (ByteString)
import Data.Data             (Data, Typeable)
import Data.Text             (Text)
import Data.Word             (Word64)
import GHC.Generics          (Generic)
import Network.HTTP.Types    (ByteRange(..), HttpVersion(..), Method(..), Query, ResponseHeaders, RequestHeaders(..), Status(..))
import Network.Socket        (SockAddr)
import qualified Pipes.Parse as Pp

------------------------------------------------------------------------------
-- Request
------------------------------------------------------------------------------

data RequestBodyLength
    = ChunkedBody
    | KnownLength Word64
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- deriving instance Show ByteRange

-- | the Request with as little parsing as possible done
data RequestRaw = RequestRaw
  { _rrMethod        :: !Method
  , _rrRequestUri    :: !ByteString
  , _rrHttpVersion   :: !HttpVersion
  , _rrHeaders       :: !RequestHeaders
  }
  deriving (Show, Typeable, Generic)

data Request = Request
  { _rqRequestRaw     :: RequestRaw
{-
  , _rqSecure         :: !Bool
    , _rqMethod         :: !Method
    , _rqRawPathInfo    :: !ByteString
    , _rqRawQueryString :: !ByteString
    , _rqVersion        :: !HttpVersion
    , _rqHeaders        :: !RequestHeaders
    , _rqPeer           :: !SockAddr
    , _rqPathInfo       :: ![Text]
    , _rqQuery          :: !Query
    , _rqCookies        :: ![(Text, Text)]
-}
    , _rqBodyLength     :: !RequestBodyLength
{-
    , _rqHeaderHost     :: !ByteString
    , _rqHeaderRange    :: !(Maybe ByteRange)
-}
  }
  deriving (Show, Typeable, Generic)

------------------------------------------------------------------------------
-- Response
------------------------------------------------------------------------------

data Response m = Response
    { _rsStatus  :: !Status
    , _rsHeaders :: !ResponseHeaders
    , _rsBody    :: !(ResponseBody m)
    , _rsClose   :: Bool
    }

data ResponseBody m
    = ResponseProducer !(Producer ByteString m ())

type RequestParser e m = Pp.Parser ByteString m (Maybe (Either e RequestRaw))
