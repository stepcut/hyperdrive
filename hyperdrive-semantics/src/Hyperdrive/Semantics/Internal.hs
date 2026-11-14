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
