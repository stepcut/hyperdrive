{-# LANGUAGE OverloadedStrings #-}
-- | Encoding field blocks (RFC 7541), with the policy of
-- docs/http2-core-design.md, "HPACK's encoder" (H1-H4):
--
-- * H1: only fields whose names are in 'indexedNames' enter the
--   dynamic table; the others are literals without indexing.
-- * H2: the fields in 'neverIndexedNames' are never-indexed literals.
-- * H3: a string is Huffman-coded only when that is strictly shorter.
-- * H4: the table's size is the smaller of the peer's setting and our
--   cap; after the peer's setting changes, the next block starts with
--   the smallest size since the last block, then the current one.
--
-- A field whose name and value are both in a table is sent as an
-- index (except a never-indexed one); otherwise its name is taken from
-- the static table if it is there, then from the dynamic table.
-- Names must already be in lower case: that is the response mapping's
-- job (D11), not the encoder's.
module Hyperdrive.HPACK.Encode
  ( Encoder
  , newEncoder
  , encoderTable
  , setPeerTableSize
  , encodeBlock
  , indexedNames
  , neverIndexedNames
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Data.List (elemIndex, findIndex)
import qualified Data.Map.Strict as M
import Data.Word (Word8)
import qualified Data.ByteString.Unsafe as BU
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Hyperdrive.HPACK.Huffman (huffmanLength, pokeHuffman)
import Hyperdrive.HPACK.Integer (integerLength, pokeInteger)
import Hyperdrive.HPACK.Table

-- | The encoder's state: its copy of the peer's dynamic table, our cap
-- on the table's size, the peer's setting, and the size updates owed
-- at the start of the next block (the smallest since the last block,
-- and the current).
data Encoder = Encoder
  { encoderTable :: !DynamicTable
  , encCap       :: !Int
  , encPeer      :: !Int
  , encPending   :: !(Maybe (Int, Int))
  } deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc7541#section-4.2
-- # An encoder can choose to use less capacity than this maximum size
-- # (see Section 6.3), but the chosen size MUST stay lower than or equal
-- # to the maximum set by the protocol.

-- | An encoder with the given cap on its table, for a peer with
-- HTTP/2's initial SETTINGS_HEADER_TABLE_SIZE, 4,096. The peer's table
-- starts at that size, so a smaller cap is signalled in the first
-- block.
newEncoder :: Int -> Encoder
newEncoder cap = Encoder (emptyTable size) cap 4096 pending
  where
    size = min cap 4096
    pending = if size < 4096 then Just (size, size) else Nothing

-- = https://www.rfc-editor.org/rfc/rfc7541#section-4.2
-- # In the case that this size is
-- # changed more than once in this interval, the smallest maximum table
-- # size that occurs in that interval MUST be signaled in a dynamic table
-- # size update.

-- | The peer's SETTINGS_HEADER_TABLE_SIZE, once acknowledged. The next
-- block will start with size updates even if the size we choose did not
-- change: an unneeded update is valid, and a missing one is an error at
-- the peer (H4).
setPeerTableSize :: Int -> Encoder -> Encoder
setPeerTableSize peer e = e { encPeer = peer, encPending = Just (smallest, size) }
  where
    size = min peer (encCap e)
    smallest = maybe size (min size . fst) (encPending e)

-- | The names whose fields enter the dynamic table (H1): their values
-- repeat from response to response.
indexedNames :: [ByteString]
indexedNames =
  [ ":status", "content-type", "server", "cache-control", "vary", "content-encoding"
  , "date", "alt-svc", "strict-transport-security", "x-content-type-options"
  , "access-control-allow-origin" ]

-- = https://www.rfc-editor.org/rfc/rfc7541#section-7.1.3
-- # An encoder might also choose not to index values for header fields
-- # that are considered to be highly valuable or sensitive to recovery,
-- # such as the Cookie or Authorization header fields.

-- | The names whose fields are always never-indexed literals (H2).
neverIndexedNames :: [ByteString]
neverIndexedNames = ["set-cookie", "www-authenticate", "proxy-authenticate", "authentication-info"]

data Rep
  = Update !Int
  | Index !Int
  | Literal !Int !Word8 !Int !ByteString !ByteString
    -- ^ the prefix, its pattern, the name's index (0: a literal name),
    -- the name and the value

staticFull :: M.Map (ByteString, ByteString) Int
staticFull = M.fromListWith min (zip staticTable [1 ..])

staticName :: M.Map ByteString Int
staticName = M.fromListWith min (zip (map fst staticTable) [1 ..])

-- = https://www.rfc-editor.org/rfc/rfc7541#section-2.1
-- # An encoder MUST order header field representations in the header
-- # block according to their ordering in the original header list.

-- | Encode a field list as one block.
encodeBlock :: Encoder -> [(ByteString, ByteString)] -> (ByteString, Encoder)
encodeBlock e fields = (render reps, e { encoderTable = table', encPending = Nothing })
  where
    (updates, table0) = case encPending e of
      Nothing -> ([], encoderTable e)
      Just (s, f)
        | s == f    -> ([Update f], resizeTable f (encoderTable e))
        | otherwise -> ([Update s, Update f], resizeTable f (resizeTable s (encoderTable e)))
    (fieldReps, table') = represent table0 fields
    reps = updates ++ fieldReps

represent :: DynamicTable -> [(ByteString, ByteString)] -> ([Rep], DynamicTable)
represent t [] = ([], t)
represent t ((n, v) : rest) =
  let (r, t') = representOne t n v
      (rs, t'') = represent t' rest
  in (r : rs, t'')

representOne :: DynamicTable -> ByteString -> ByteString -> (Rep, DynamicTable)
representOne t n v
  -- = https://www.rfc-editor.org/rfc/rfc7541#section-6.2.3
  -- # When a header field is represented as a literal header field never
  -- # indexed, it MUST always be encoded with this specific literal
  -- # representation.
  | n `elem` neverIndexedNames = (Literal 4 0x10 nameIndex n v, t)
  | Just i <- M.lookup (n, v) staticFull = (Index i, t)
  | Just i <- elemIndex (n, v) entries = (Index (staticTableLength + 1 + i), t)
  | n `elem` indexedNames = (Literal 6 0x40 nameIndex n v, insertEntry n v t)
  | otherwise = (Literal 4 0x00 nameIndex n v, t)
  where
    entries = tableEntries t
    nameIndex = case M.lookup n staticName of
      Just i  -> i
      Nothing -> maybe 0 (\i -> staticTableLength + 1 + i) (findIndex ((== n) . fst) entries)

-- the octets a string literal takes, and whether it is Huffman-coded
-- (H3: only when strictly shorter)
stringLength :: ByteString -> (Int, Bool)
stringLength s =
  let h = huffmanLength s
      huffman = h < B.length s
      len = if huffman then h else B.length s
  in (integerLength 7 len + len, huffman)

repLength :: Rep -> Int
repLength (Update m) = integerLength 5 m
repLength (Index i) = integerLength 7 i
repLength (Literal prefix _ ni n v) =
  integerLength prefix ni + (if ni == 0 then fst (stringLength n) else 0) + fst (stringLength v)

render :: [Rep] -> ByteString
render reps = BI.unsafeCreate (sum (map repLength reps)) $ \p -> () <$ pokeReps reps p

pokeReps :: [Rep] -> Ptr Word8 -> IO (Ptr Word8)
pokeReps [] p = pure p
pokeReps (r : rs) p = pokeRep r p >>= pokeReps rs

pokeRep :: Rep -> Ptr Word8 -> IO (Ptr Word8)
pokeRep (Update m) p = pokeInteger 5 0x20 m p
pokeRep (Index i) p = pokeInteger 7 0x80 i p
pokeRep (Literal prefix pattern ni n v) p = do
  p1 <- pokeInteger prefix pattern ni p
  p2 <- if ni == 0 then pokeString n p1 else pure p1
  pokeString v p2

pokeString :: ByteString -> Ptr Word8 -> IO (Ptr Word8)
pokeString s p
  | snd (stringLength s) = pokeInteger 7 0x80 (huffmanLength s) p >>= pokeHuffman s
  | otherwise = do
      q <- pokeInteger 7 0x00 (B.length s) p
      BU.unsafeUseAsCString s $ \src -> copyBytes q (castPtr src) (B.length s)
      pure (q `plusPtr` B.length s)
