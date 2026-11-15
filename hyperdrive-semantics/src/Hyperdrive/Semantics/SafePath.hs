{-# LANGUAGE OverloadedStrings #-}
-- | Path segments that are safe to use as file or directory names.
--
-- 'Hyperdrive.Semantics.URI.pathSegments' decodes a path faithfully, so
-- a segment may be @..@, or may contain a "/" that was sent encoded as
-- @%2F@. Joining such segments into a file system path lets a request
-- climb out of the directory it was meant to stay in:
--
-- > /static/%2E%2E%2Fsecret  ==>  ["static", "../secret"]
--
-- A 'SafeSegment' can only be built from a segment that cannot do
-- that, so a sequence of them can be joined with "/" to name a file
-- beneath some root.
--
-- These checks are enough for POSIX file systems. Windows gives
-- special meaning to more names and characters, such as @\\@, @:@ and
-- device names like @aux@; code serving files from Windows must also
-- reject those.
module Hyperdrive.Semantics.SafePath
  ( SafeSegment
  , UnsafeSegment(..)
  , mkSafeSegment
  , safeSegmentBytes
  , safePathSegments
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Hyperdrive.Semantics.Internal (SafeSegment(..))
import Hyperdrive.Semantics.URI (Path, PathSegmentError(..), pathSegments)

data UnsafeSegment
  = DotSegment       -- ^ the segment was @.@ or @..@
  | ContainsSlash    -- ^ the segment contained "/", which was sent as @%2F@
  | ContainsNUL      -- ^ the segment contained NUL, which was sent as @%00@
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-17.3
-- # Therefore, an origin server needs to avoid accessing
-- # names that have a special significance to the system when mapping the
-- # target resource to files, folders, or directories.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-17.3
-- # For example, UNIX, Microsoft Windows, and other operating systems use
-- # ".." as a path component to indicate a directory level above the
-- # current one

-- = https://www.rfc-editor.org/rfc/rfc3986#section-7.3
-- # Security checks of the data within a URI should be applied after
-- # decoding the octets.

-- | Check a decoded segment. An empty segment is allowed: it names
-- nothing, and joining it produces "//", which file systems treat as
-- "/".
mkSafeSegment :: ByteString -> Either UnsafeSegment SafeSegment
mkSafeSegment s
  | s == "." || s == ".." = Left DotSegment
  | B.elem 0x2F s         = Left ContainsSlash
  | B.elem 0x00 s         = Left ContainsNUL
  | otherwise             = Right (SafeSegment s)

safeSegmentBytes :: SafeSegment -> ByteString
safeSegmentBytes (SafeSegment s) = s

-- | Decode a path into segments that are all safe to use as file names.
safePathSegments :: Path -> Either UnsafeSegment [SafeSegment]
safePathSegments p =
  case pathSegments p of
    Left SegmentContainsNUL -> Left ContainsNUL
    Right segs              -> traverse mkSafeSegment segs
