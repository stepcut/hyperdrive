-- | Raw constructors for the semantic types.
--
-- The public modules only hand out values that satisfy each type's
-- invariant. These constructors bypass those checks and exist so that
-- protocol parsers which have /already/ validated their input do not
-- pay for a second pass. Anything built with them must satisfy the
-- invariant documented on the type.
module Hyperdrive.Semantics.Internal
  ( Method(..)
  , FieldName(..)
  , FieldValue(..)
  , Fields(..)
  , Status(..)
  , Scheme(..)
  , RegName(..)
  , IPvFuture(..)
  , Path(..)
  , Query(..)
  ) where

import Data.ByteString (ByteString)

-- | Invariant: the bytes are a @token@.
newtype Method = Method ByteString
  deriving (Eq, Ord, Show)

-- | Invariant: the bytes are a @token@ containing no upper case
-- letters.
newtype FieldName = FieldName ByteString
  deriving (Eq, Ord, Show)

-- | Invariant: the bytes match @field-value@; in particular there is
-- no leading or trailing whitespace and no CR, LF or NUL.
newtype FieldValue = FieldValue ByteString
  deriving (Eq, Ord, Show)

-- | A field section (headers or trailers), in the order received.
newtype Fields = Fields [(FieldName, FieldValue)]
  deriving (Eq, Show)

-- | Invariant: @100 <= code <= 599@.
newtype Status = Status Int
  deriving (Eq, Ord, Show)

-- | Invariant: the bytes match @scheme@ and are in lower case.
newtype Scheme = Scheme ByteString
  deriving (Eq, Ord, Show)

-- | Invariant: the bytes match @reg-name@, are not empty, have no
-- upper case letters, except that the hex digits of percent-encodings
-- are upper case.
newtype RegName = RegName ByteString
  deriving (Eq, Ord, Show)

-- | Invariant: the bytes match @IPvFuture@ and are in lower case.
newtype IPvFuture = IPvFuture ByteString
  deriving (Eq, Ord, Show)

-- | Invariant: the bytes match @path-abempty@.
newtype Path = Path ByteString
  deriving (Eq, Ord, Show)

-- | Invariant: the bytes match @query@.
newtype Query = Query ByteString
  deriving (Eq, Ord, Show)
