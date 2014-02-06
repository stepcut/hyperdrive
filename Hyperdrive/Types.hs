{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, StandaloneDeriving, TemplateHaskell, OverloadedStrings #-}
module Hyperdrive.Types where

import Pipes              (Producer)
import Data.ByteString    (ByteString)
import Data.Data          (Data, Typeable)
import Data.Text          (Text)
import Data.Word          (Word64)
import GHC.Generics       (Generic)
import Network.HTTP.Types (ByteRange(..), HttpVersion(..), Method(..), Query, ResponseHeaders, RequestHeaders(..), Status(..))
import Network.Socket     (SockAddr)

------------------------------------------------------------------------------
-- Request
------------------------------------------------------------------------------

data RequestBodyLength
    = ChunkedBody
    | KnownLength Word64
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

deriving instance Show ByteRange

data Request = Request
    { _rqSecure         :: !Bool
    , _rqMethod         :: !Method
    , _rqRawPathInfo    :: !ByteString
    , _rqRawQueryString :: !ByteString
    , _rqVersion        :: !HttpVersion
    , _rqHeaders        :: !RequestHeaders
    , _rqPeer           :: !SockAddr
    , _rqPathInfo       :: ![Text]
    , _rqQuery          :: !Query
    , _rqCookies        :: ![(Text, Text)]
    , _rqBodyLength     :: !RequestBodyLength
    , _rqHeaderHost     :: !ByteString
    , _rqHeaderRange    :: !(Maybe ByteRange)
    }
    deriving (Show, Typeable, Generic)

------------------------------------------------------------------------------
-- Response
------------------------------------------------------------------------------

data Response m = Response
    { _rsStatus  :: !Status
    , _rsHeaders :: !ResponseHeaders
    , _rsBody    :: !(ResponseBody m)
    }

data ResponseBody m
    = ResponseProducer !(Producer ByteString m ())
