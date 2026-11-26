{-# LANGUAGE OverloadedStrings #-}
module EncodeTests (encodeTests, genFields) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import DecodeTests (Kind(..), NameRep(..), Rep(..))
import Hyperdrive.HPACK.Decode
import Hyperdrive.HPACK.Encode
import Hyperdrive.HPACK.Huffman (decodeHuffman, huffmanLength)
import Hyperdrive.HPACK.Integer (DecodedInteger(..), decodeInteger)
import Hyperdrive.HPACK.Table
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

encodeTests :: TestTree
encodeTests = testGroup "Encoding field blocks (docs/http2-core-design.md, H1-H4)"
  [ testProperty "the decoder gets the fields back, and both sides' tables agree" prop_roundTrip
  , testProperty "each field's representation follows H1 and H2, each string H3" prop_policy
  , testProperty "size updates follow the peer's setting (H4)" prop_updates
  , testProperty "examples" prop_examples
  ]

-- | Fields as a server's responses have them: names from the indexed
-- and never-indexed lists, the static table and elsewhere, with values
-- that often repeat.
genFields :: Gen [(ByteString, ByteString)]
genFields = listOf ((,) <$> name <*> value)
  where
    name = oneof
      [ elements indexedNames, elements neverIndexedNames
      , elements (map fst staticTable)
      , elements ["content-length", "etag", "location", "x-custom", "x-request-id"] ]
    value = frequency
      [ (5, elements ["200", "302", "private", "text/html", "gzip", "", "Mon, 21 Oct 2013 20:13:21 GMT"])
      , (2, B.pack <$> listOf arbitrary)
      , (1, (`C.replicate` 'x') <$> choose (100, 5000)) ]

data Step = SetPeer Int | Block [(ByteString, ByteString)]
  deriving (Show)

genSteps :: Gen [Step]
genSteps = listOf (frequency
  [ (1, SetPeer <$> frequency [(3, choose (0, 600)), (2, choose (0, 8192)), (1, pure 4096)])
  , (4, Block <$> genFields) ])

-- = https://www.rfc-editor.org/rfc/rfc7541#section-2.1
-- # An encoder MUST order header field representations in the header
-- # block according to their ordering in the original header list.
--
-- = https://www.rfc-editor.org/rfc/rfc7541#section-4.2
-- # An encoder can choose to use less capacity than this maximum size
-- # (see Section 6.3), but the chosen size MUST stay lower than or equal
-- # to the maximum set by the protocol.
prop_roundTrip :: Property
prop_roundTrip = forAll (elements [0, 100, 256, 4096, 10000]) $ \cap ->
  forAll genSteps $ \steps ->
    go (newEncoder cap) (newDecoder 4096 4096) 4096 cap steps
  where
    go _ _ _ _ [] = property True
    go e d _ cap (SetPeer p : rest) = go (setPeerTableSize p e) (setDecoderLimit p d) p cap rest
    go e d peer cap (Block fs : rest) =
      let (block, e') = encodeBlock e fs
      in case decodeBlock maxBound d block of
           Left err -> counterexample (show err) False
           Right (decoded, d') ->
             decoded === Fields fs
             .&&. encoderTable e' === decoderTable d'
             .&&. counterexample "table above the peer's setting or the cap"
                    (tableMaxSize (encoderTable e') <= min peer cap)
             .&&. go e' d' peer cap rest

-- | The representations of a block, as Section 6 defines them.
parseBlock :: ByteString -> [Rep]
parseBlock = go . B.unpack
  where
    go [] = []
    go os@(b : _)
      | b >= 0x80 = let (i, rest) = int 7 os in Indexed i : go rest
      | b >= 0x40 = lit WithIndexing 6 os
      | b >= 0x20 = let (m, rest) = int 5 os in SizeUpdate m : go rest
      | b >= 0x10 = lit NeverIndexed 4 os
      | otherwise = lit WithoutIndexing 4 os
    lit kind prefix os =
      let (i, rest) = int prefix os
          (name, rest') = if i == 0 then let ((n, h), r) = str rest in (NameLiteral n h, r)
                          else (NameIndex i, rest)
          ((v, hv), rest'') = str rest'
      in Literal kind name v hv : go rest''
    int n os = case decodeInteger n (B.pack os) 0 of
      Decoded v k -> (v, drop k os)
      _ -> error "bad integer"
    str os@(b : _) =
      let (len, rest) = int 7 os
          raw = B.pack (take len rest)
          h = b >= 0x80
      in ((if h then either (error . show) id (decodeHuffman raw) else raw, h), drop len rest)
    str [] = error "no string"

prop_policy :: Property
prop_policy = forAll genFields $ \fs ->
  let (block, _) = encodeBlock (newEncoder 4096) fs
      reps = parseBlock block
  in length reps === length fs
     .&&. conjoin (zipWith check fs reps)
  where
    check (n, v) r = counterexample (show ((n, v), r)) $ case r of
      Indexed _ -> property (n `notElem` neverIndexedNames)
      Literal kind name value hv ->
        kind === expectedKind n v .&&. value === v .&&. huffmanRule v hv
        .&&. case name of
               NameLiteral l hl -> l === n .&&. huffmanRule l hl
                 .&&. counterexample "a literal name that is in the static table"
                        (n `notElem` map fst staticTable)
               NameIndex _ -> property True
      SizeUpdate _ -> counterexample "a size update without a change" False
    expectedKind n v
      | n `elem` neverIndexedNames = NeverIndexed
      | n `elem` indexedNames, B.length n + B.length v + 32 <= 4096 `div` 4 = WithIndexing
      | otherwise = WithoutIndexing
    huffmanRule s h = counterexample "Huffman only when strictly shorter" $
      h === (huffmanLength s < B.length s)

-- = https://www.rfc-editor.org/rfc/rfc7541#section-4.2
-- # In the case that this size is
-- # changed more than once in this interval, the smallest maximum table
-- # size that occurs in that interval MUST be signaled in a dynamic table
-- # size update.
prop_updates :: Property
prop_updates = forAll (elements [0, 100, 4096, 10000]) $ \cap ->
  forAll (listOf (choose (0, 8192))) $ \settings ->
    let e = foldl (flip setPeerTableSize) (newEncoder cap) settings
        (block, e') = encodeBlock e [(":status", "200")]
        (block2, _) = encodeBlock e' [(":status", "200")]
        sizes = map (min cap) settings
        initial = [min cap 4096 | cap < 4096]
        expected = case (initial, sizes) of
          ([], []) -> []
          (_, []) -> initial
          _ -> let s = minimum (initial ++ sizes); f = last sizes
               in if s == f then [f] else [s, f]
    in [m | SizeUpdate m <- parseBlock block] === expected
       .&&. [m | SizeUpdate m <- parseBlock block2] === []

prop_examples :: Property
prop_examples = once $ conjoin
  [ enc [(":status", "200")] === [0x88]
    -- RFC 7541 Appendix C.6.1 encodes these the same way
  , enc [(":status", "302")] === [0x48, 0x82, 0x64, 0x02]
  , enc [("cache-control", "private")] === [0x58, 0x85, 0xae, 0xc3, 0x77, 0x1a, 0x4b]
    -- a repeated field is an index into the dynamic table
  , fst (encodeBlock (snd (encodeBlock e0 [("server", "hyperdrive")])) [("server", "hyperdrive")])
      === B.pack [0xBE]
    -- not indexed: content-length (static index 28) without indexing
  , enc [("content-length", "0")] === [0x0F, 0x0D, 0x01, 0x30]
    -- never indexed: set-cookie (static index 55)
  , take 2 (enc [("set-cookie", "a=b")]) === [0x1F, 0x28]
  ]
  where
    e0 = newEncoder 4096
    enc = B.unpack . fst . encodeBlock e0
