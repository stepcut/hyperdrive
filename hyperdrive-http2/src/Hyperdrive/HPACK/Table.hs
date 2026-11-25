{-# LANGUAGE OverloadedStrings #-}
-- | HPACK's static and dynamic tables (RFC 7541, Sections 2.3 and 4).
--
-- The dynamic table is a persistent sequence, newest entry first, so
-- that a connection's state stays a pure value (the core is sans-IO).
-- Entries are copied when they are added: they live as long as the
-- connection, and a slice would keep the whole input buffer alive.
module Hyperdrive.HPACK.Table
  ( -- * The static table
    staticTable
  , staticTableLength
  , staticEntry
    -- * The dynamic table
  , DynamicTable
  , emptyTable
  , tableEntries
  , tableSize
  , tableMaxSize
  , entrySize
  , insertEntry
  , resizeTable
    -- * Both
  , lookupIndex
  ) where

import Data.Array (Array, listArray, (!))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (toList)
import Data.Sequence (Seq, ViewR(..), viewr, (<|))
import qualified Data.Sequence as Seq

-- | The static table, from RFC 7541 Appendix A: index 1 first.
staticTable :: [(ByteString, ByteString)]
staticTable =
  [ (":authority",                  "")
  , (":method",                     "GET")
  , (":method",                     "POST")
  , (":path",                       "/")
  , (":path",                       "/index.html")
  , (":scheme",                     "http")
  , (":scheme",                     "https")
  , (":status",                     "200")
  , (":status",                     "204")
  , (":status",                     "206")
  , (":status",                     "304")
  , (":status",                     "400")
  , (":status",                     "404")
  , (":status",                     "500")
  , ("accept-charset",              "")
  , ("accept-encoding",             "gzip, deflate")
  , ("accept-language",             "")
  , ("accept-ranges",               "")
  , ("accept",                      "")
  , ("access-control-allow-origin", "")
  , ("age",                         "")
  , ("allow",                       "")
  , ("authorization",               "")
  , ("cache-control",               "")
  , ("content-disposition",         "")
  , ("content-encoding",            "")
  , ("content-language",            "")
  , ("content-length",              "")
  , ("content-location",            "")
  , ("content-range",               "")
  , ("content-type",                "")
  , ("cookie",                      "")
  , ("date",                        "")
  , ("etag",                        "")
  , ("expect",                      "")
  , ("expires",                     "")
  , ("from",                        "")
  , ("host",                        "")
  , ("if-match",                    "")
  , ("if-modified-since",           "")
  , ("if-none-match",               "")
  , ("if-range",                    "")
  , ("if-unmodified-since",         "")
  , ("last-modified",               "")
  , ("link",                        "")
  , ("location",                    "")
  , ("max-forwards",                "")
  , ("proxy-authenticate",          "")
  , ("proxy-authorization",         "")
  , ("range",                       "")
  , ("referer",                     "")
  , ("refresh",                     "")
  , ("retry-after",                 "")
  , ("server",                      "")
  , ("set-cookie",                  "")
  , ("strict-transport-security",   "")
  , ("transfer-encoding",           "")
  , ("user-agent",                  "")
  , ("vary",                        "")
  , ("via",                         "")
  , ("www-authenticate",            "")
  ]

staticTableLength :: Int
staticTableLength = 61

staticArray :: Array Int (ByteString, ByteString)
staticArray = listArray (1, staticTableLength) staticTable

-- | The static table's entry at an index from 1 to 61.
staticEntry :: Int -> (ByteString, ByteString)
staticEntry i = staticArray ! i

data Entry = Entry !ByteString !ByteString

-- | The dynamic table: its entries newest first, their total size, and
-- the maximum size set by the last dynamic table size update.
data DynamicTable = DynamicTable
  { dtEntries :: !(Seq Entry)
  , dtSize    :: !Int
  , dtMaxSize :: !Int
  }

instance Eq DynamicTable where
  a == b = tableEntries a == tableEntries b && dtMaxSize a == dtMaxSize b

instance Show DynamicTable where
  show t = "DynamicTable " ++ show (dtMaxSize t) ++ " " ++ show (tableEntries t)

-- = https://www.rfc-editor.org/rfc/rfc7541#section-2.3.2
-- # The dynamic table is initially empty.

-- | An empty table with the given maximum size.
emptyTable :: Int -> DynamicTable
emptyTable = DynamicTable Seq.empty 0

-- | The entries, newest (index 62) first.
tableEntries :: DynamicTable -> [(ByteString, ByteString)]
tableEntries t = [(n, v) | Entry n v <- toList (dtEntries t)]

tableSize :: DynamicTable -> Int
tableSize = dtSize

tableMaxSize :: DynamicTable -> Int
tableMaxSize = dtMaxSize

-- = https://www.rfc-editor.org/rfc/rfc7541#section-4.1
-- # The size of an entry is the sum of its name's length in octets (as
-- # defined in Section 5.2), its value's length in octets, and 32.

entrySize :: ByteString -> ByteString -> Int
entrySize n v = B.length n + B.length v + 32

-- = https://www.rfc-editor.org/rfc/rfc7541#section-4.4
-- # Before a new entry is added to the dynamic table, entries are evicted
-- # from the end of the dynamic table until the size of the dynamic table
-- # is less than or equal to (maximum size - new entry size) or until the
-- # table is empty.
-- #
-- # If the size of the new entry is less than or equal to the maximum
-- # size, that entry is added to the table.  It is not an error to
-- # attempt to add an entry that is larger than the maximum size; an
-- # attempt to add an entry larger than the maximum size causes the table
-- # to be emptied of all existing entries and results in an empty table.

-- = https://www.rfc-editor.org/rfc/rfc7541#section-2.3.2
-- # Therefore, duplicate entries MUST NOT
-- # be treated as an error by a decoder.

-- | Add an entry at the start, evicting from the end to make room. An
-- entry equal to one already there is added like any other.
insertEntry :: ByteString -> ByteString -> DynamicTable -> DynamicTable
insertEntry n v t
  | size > dtMaxSize t = t { dtEntries = Seq.empty, dtSize = 0 }
  | otherwise =
      let t' = evictTo (dtMaxSize t - size) t
      in t' { dtEntries = Entry (B.copy n) (B.copy v) <| dtEntries t'
            , dtSize = dtSize t' + size }
  where
    size = entrySize n v

-- = https://www.rfc-editor.org/rfc/rfc7541#section-4.3
-- # Whenever the maximum size for the dynamic table is reduced, entries
-- # are evicted from the end of the dynamic table until the size of the
-- # dynamic table is less than or equal to the maximum size.

-- | Set a new maximum size, evicting from the end to fit it.
resizeTable :: Int -> DynamicTable -> DynamicTable
resizeTable m t = (evictTo m t) { dtMaxSize = m }

evictTo :: Int -> DynamicTable -> DynamicTable
evictTo target t
  | dtSize t <= target = t
  | otherwise = case viewr (dtEntries t) of
      EmptyR -> t
      rest :> Entry n v -> evictTo target t { dtEntries = rest, dtSize = dtSize t - entrySize n v }

-- = https://www.rfc-editor.org/rfc/rfc7541#section-2.3.3
-- # Indices between 1 and the length of the static table (inclusive)
-- # refer to elements in the static table (see Section 2.3.1).
-- #
-- # Indices strictly greater than the length of the static table refer to
-- # elements in the dynamic table (see Section 2.3.2).  The length of the
-- # static table is subtracted to find the index into the dynamic table.
-- #
-- # Indices strictly greater than the sum of the lengths of both tables
-- # MUST be treated as a decoding error.

-- | The entry at an index of the combined address space, or 'Nothing'
-- if the index is 0 or beyond both tables.
lookupIndex :: Int -> DynamicTable -> Maybe (ByteString, ByteString)
lookupIndex i t
  | i <= 0 = Nothing
  | i <= staticTableLength = Just (staticEntry i)
  | otherwise = case Seq.lookup (i - staticTableLength - 1) (dtEntries t) of
      Just (Entry n v) -> Just (n, v)
      Nothing          -> Nothing
