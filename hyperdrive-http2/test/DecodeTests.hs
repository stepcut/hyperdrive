{-# LANGUAGE OverloadedStrings #-}
module DecodeTests (decodeTests, Rep(..), Kind(..), NameRep(..), encodeRep, Model(..), genBlock) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Word (Word8)
import Hyperdrive.HPACK.Decode
import Hyperdrive.HPACK.Huffman (HuffmanError(..), decodeHuffman, encodeHuffman)
import Hyperdrive.HPACK.Integer (DecodedInteger(..), decodeInteger, encodeInteger)
import Hyperdrive.HPACK.Table
import RFC7541
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

decodeTests :: String -> TestTree
decodeTests text = testGroup "Decoding field blocks (RFC 7541 Sections 2 to 6)"
  [ testProperty "the static table is Appendix A's" (prop_static text)
  , testProperty "Appendix C's examples: fields, table entries and sizes" (prop_appendixC text)
  , testProperty "agrees with a reference decoder, block after block on one connection" prop_reference
  , testProperty "agrees with a reference decoder on damaged blocks" prop_damaged
  , testProperty "a list over the limit is refused, and the table updated as without a limit" prop_listLimit
  , testProperty "errors and limits, by example" prop_errors
  ]

prop_static :: String -> Property
prop_static text = once $
  [(i, C.unpack n, C.unpack v) | (i, (n, v)) <- zip [1 ..] staticTable] === staticTableRows text
  .&&. length staticTable === staticTableLength

------------------------------------------------------------------------------
-- Appendix C
------------------------------------------------------------------------------

prop_appendixC :: String -> Property
prop_appendixC text = once $
  length (concatMap snd groups) === 16
  .&&. conjoin [ runGroup (newDecoder size size) examples | (size, examples) <- groups ]
  where
    groups = appendixC text
    runGroup _ [] = property True
    runGroup d (e : es) = counterexample (exampleName e) $
      case decodeBlock maxBound d (B.pack (exampleEncoded e)) of
        Left err -> counterexample (show err) False
        Right (decoded, d') ->
          let t = decoderTable d'
          in decoded === Fields [(C.pack n, C.pack v) | (n, v) <- exampleFields e]
             .&&. tableEntries t === [(C.pack n, C.pack v) | (_, (n, v)) <- exampleTable e]
             .&&. [entrySize (C.pack n) (C.pack v) | (_, (n, v)) <- exampleTable e]
                    === map fst (exampleTable e)
             .&&. tableSize t === exampleTableSize e
             .&&. runGroup d' es

------------------------------------------------------------------------------
-- representations, written as Section 6 draws them
------------------------------------------------------------------------------

data Kind = WithIndexing | WithoutIndexing | NeverIndexed
  deriving (Eq, Show)

data NameRep = NameIndex Int | NameLiteral ByteString Bool  -- ^ the name, Huffman-coded or not
  deriving (Eq, Show)

data Rep
  = Indexed Int
  | Literal Kind NameRep ByteString Bool  -- ^ the value, Huffman-coded or not
  | SizeUpdate Int
  deriving (Eq, Show)

encodeRep :: Rep -> ByteString
encodeRep (Indexed i) = encodeInteger 7 0x80 i
encodeRep (SizeUpdate m) = encodeInteger 5 0x20 m
encodeRep (Literal kind name value h) = nameRep <> stringLiteral value h
  where
    (prefix, pattern) = case kind of
      WithIndexing    -> (6, 0x40)
      WithoutIndexing -> (4, 0x00)
      NeverIndexed    -> (4, 0x10)
    nameRep = case name of
      NameIndex i       -> encodeInteger prefix pattern i
      NameLiteral n hn  -> encodeInteger prefix pattern 0 <> stringLiteral n hn

stringLiteral :: ByteString -> Bool -> ByteString
stringLiteral s h
  | h = let e = encodeHuffman s in encodeInteger 7 0x80 (B.length e) <> e
  | otherwise = encodeInteger 7 0 (B.length s) <> s

------------------------------------------------------------------------------
-- the reference decoder
------------------------------------------------------------------------------

-- | The table as a list, newest first, and its maximum size.
data Model = Model Int [(ByteString, ByteString)]
  deriving (Eq, Show)

modelSize :: [(ByteString, ByteString)] -> Int
modelSize es = sum [B.length n + B.length v + 32 | (n, v) <- es]

-- Section 4.4: evict from the end until the new entry fits, or the
-- table is empty; add it if it fits
modelInsert :: (ByteString, ByteString) -> Model -> Model
modelInsert e@(n, v) (Model m es) =
  let s = B.length n + B.length v + 32
      kept = evict (m - s) es
  in Model m (if s <= m then e : kept else kept)
  where
    evict target xs
      | modelSize xs <= target || null xs = xs
      | otherwise = evict target (init xs)

-- Section 4.3
modelResize :: Int -> Model -> Model
modelResize m (Model _ es) = Model m (go es)
  where go xs | modelSize xs <= m = xs
              | otherwise = go (init xs)

modelLookup :: Int -> Model -> Maybe (ByteString, ByteString)
modelLookup i (Model _ es)
  | i >= 1 && i <= length staticTable = Just (staticTable !! (i - 1))
  | i > length staticTable && i - length staticTable <= length es = Just (es !! (i - length staticTable - 1))
  | otherwise = Nothing

-- | Sections 3.2 and 6, one representation at a time.
reference :: Int -> Int -> Model -> ByteString -> Either DecodeError (Decoded, Model)
reference limit maxList model0 = go model0 [] True . B.unpack
  where
    go model fields _ [] =
      let size = sum [B.length n + B.length v + 32 | (n, v) <- fields]
      in Right (if size > maxList then ListTooLarge else Fields fields, model)
    go model fields atStart os@(b : _)
      | b >= 0x80 = do
          (i, rest) <- int 7 os
          if i == 0 then Left IndexZero else do
            e <- maybe (Left (IndexOutOfRange i)) Right (modelLookup i model)
            go model (fields ++ [e]) False rest
      | b >= 0x40 = lit True 6 model fields os
      | b >= 0x20 =
          if not atStart then Left TableSizeUpdateLate
          else do
            (m, rest) <- int 5 os
            if m > limit then Left (TableSizeAboveLimit m)
              else go (modelResize m model) fields True rest
      | otherwise = lit False 4 model fields os
    lit adds prefix model fields os = do
      (i, rest) <- int prefix os
      (name, rest') <-
        if i == 0 then str rest
        else maybe (Left (IndexOutOfRange i)) (\(n, _) -> Right (n, rest)) (modelLookup i model)
      (value, rest'') <- str rest'
      let model' = if adds then modelInsert (name, value) model else model
      go model' (fields ++ [(name, value)]) False rest''
    int n os = case decodeInteger n (B.pack os) 0 of
      Decoded v k -> Right (v, drop k os)
      Incomplete  -> Left Truncated
      TooLarge    -> Left IntegerTooLarge
    str [] = Left Truncated
    str os@(b : _) = do
      (len, rest) <- int 7 os
      if len > length rest then Left Truncated else do
        let raw = B.pack (take len rest)
        s <- if b >= 0x80 then either (Left . BadHuffman) Right (decodeHuffman raw) else Right raw
        Right (s, drop len rest)

------------------------------------------------------------------------------
-- generated blocks
------------------------------------------------------------------------------

-- | A block for a decoder in the given state. Four in five are valid:
-- up to three size updates at the start, then fields that use every
-- representation, each index chosen within the table as it is at that
-- point. The fifth has one fault: index 0, an index beyond the tables,
-- an update above the limit, or one after a field.
genBlock :: Int -> Model -> Gen [Rep]
genBlock limit model0 = do
  updates <- frequency [(6, pure []), (3, vectorOf 1 update), (2, vectorOf 2 update), (1, vectorOf 3 update)]
  n <- choose (0, 12)
  reps <- fieldsFrom (foldl (flip modelResize) model0 [m | SizeUpdate m <- updates]) n
  let valid = updates ++ reps
  faulty <- frequency [(4, pure False), (1, pure True)]
  if not faulty then pure valid else do
    pos <- choose (length updates, length valid)
    oneof
      [ pure (take pos valid ++ [Indexed 0] ++ drop pos valid)
      , pure (take pos valid ++ [Indexed (staticTableLength + 1000)] ++ drop pos valid)
      , pure (SizeUpdate (limit + 1) : valid)
      , pure (valid ++ [Indexed 2, SizeUpdate 0]) ]
  where
    update = SizeUpdate <$> choose (0, limit)
    fieldsFrom _ 0 = pure []
    fieldsFrom model k = do
      let Model _ es = model
          index = choose (1, staticTableLength + length es)
      r <- frequency
        [ (3, Indexed <$> index)
        , (5, Literal <$> elements [WithIndexing, WithoutIndexing, NeverIndexed]
                      <*> oneof [NameIndex <$> index, NameLiteral <$> name <*> arbitrary]
                      <*> value <*> arbitrary) ]
      let model' = case r of
            Literal WithIndexing nr v _ ->
              let n = case nr of
                    NameLiteral l _ -> l
                    NameIndex i -> maybe "" fst (modelLookup i model)
              in modelInsert (n, v) model
            _ -> model
      (r :) <$> fieldsFrom model' (k - 1 :: Int)
    name = elements ["custom-key", "x", "", "set-cookie", ":path", "a-much-longer-field-name-for-eviction"]
    value = frequency
      [ (6, elements ["", "v", "custom-value", "/index.html", "gzip"])
      , (2, B.pack <$> listOf (arbitrary :: Gen Word8))
      , (1, (`C.replicate` 'x') <$> choose (100, 400)) ]

modelOf :: Decoder -> Model
modelOf d = Model (tableMaxSize (decoderTable d)) (tableEntries (decoderTable d))

-- = https://www.rfc-editor.org/rfc/rfc7541#section-2.1
-- # A decoder MUST order header fields in the decoded header list
-- # according to their ordering in the header block.
--
-- = https://www.rfc-editor.org/rfc/rfc7541#section-2.3.2
-- # Therefore, duplicate entries MUST NOT
-- # be treated as an error by a decoder.
prop_reference :: Property
prop_reference = forAll (choose (0, 600)) $ \limit ->
  forAll (choose (0, 3)) $ \nBlocks ->
    go limit (newDecoder limit limit) (nBlocks + 1 :: Int)
  where
    go _ _ 0 = property True
    go limit d k =
      forAll (genBlock limit (modelOf d)) $ \reps ->
        let block = B.concat (map encodeRep reps)
            outcome = either (takeWhile (/= ' ') . show) (const "decoded") (decodeBlock maxBound d block)
        in tabulate "outcome" [outcome] $
           tabulate "evictions" [show (any isIndexing reps && limit < 200)] $
           case (decodeBlock maxBound d block, reference limit maxBound (modelOf d) block) of
             (Right (r, d'), Right (r', m')) ->
               r === r' .&&. modelOf d' === m'
               .&&. tableSize (decoderTable d') === modelSize (tableEntries (decoderTable d'))
               .&&. go limit d' (k - 1)
             (a, b) -> fmap fst a === fmap fst b

isIndexing :: Rep -> Bool
isIndexing (Literal WithIndexing _ _ _) = True
isIndexing _ = False

prop_damaged :: Property
prop_damaged = forAll (choose (0, 600)) $ \limit ->
  forAll (genBlock limit (Model limit [])) $ \reps ->
  forAll (damage (B.concat (map encodeRep reps))) $ \block ->
    fmap (fmap modelOf) (decodeBlock maxBound (newDecoder limit limit) block)
      === reference limit maxBound (Model limit []) block
  where
    damage b = oneof
      [ (\i -> B.take i b) <$> choose (0, B.length b)
      , do i <- choose (0, max 0 (B.length b - 1))
           x <- arbitrary
           pure (B.take i b <> B.singleton x <> B.drop (i + 1) b)
      , B.pack <$> arbitrary ]

prop_listLimit :: Property
prop_listLimit = forAll (choose (0, 600)) $ \limit ->
  forAll (genBlock limit (Model limit [])) $ \reps ->
  forAll (choose (0, 2000)) $ \maxList ->
    let block = B.concat (map encodeRep reps)
        d = newDecoder limit limit
    in case (decodeBlock maxList d block, decodeBlock maxBound d block) of
         (Right (r, d1), Right (Fields fs, d2)) ->
           let size = sum [entrySize n v | (n, v) <- fs]
           in counterexample ("list size " ++ show size) $
                d1 === d2 .&&. r === (if size > maxList then ListTooLarge else Fields fs)
         (a, b) -> fmap snd a === fmap snd b

prop_errors :: Property
prop_errors = once $ conjoin
  [ dec [Indexed 0] === Left IndexZero
  , dec [Indexed 62] === Left (IndexOutOfRange 62)
  , dec [Literal WithoutIndexing (NameIndex 62) "v" False] === Left (IndexOutOfRange 62)
  , dec [SizeUpdate 4097] === Left (TableSizeAboveLimit 4097)
  , fmap fst (dec [SizeUpdate 4096]) === Right (Fields [])
  , fmap fst (dec [SizeUpdate 0, SizeUpdate 4096, Indexed 2]) === Right (Fields [(":method", "GET")])
    -- any number of updates may start a block
  , fmap (fmap (tableMaxSize . decoderTable)) (dec [SizeUpdate 0, SizeUpdate 10, SizeUpdate 20, Indexed 2])
      === Right (Fields [(":method", "GET")], 20)
  , dec [Indexed 2, SizeUpdate 100] === Left TableSizeUpdateLate
  , decodeBlock maxBound d0 (B.pack [0x40, 0x05, 0x61]) === Left Truncated
  , decodeBlock maxBound d0 (B.pack [0x82, 0xFF]) === Left Truncated
  , decodeBlock maxBound d0 (B.pack (0xFF : replicate 10 0x80 ++ [0])) === Left IntegerTooLarge
  , decodeBlock maxBound d0 (B.pack [0x00, 0x81, 0xFF, 0x00]) === Left (BadHuffman PaddingTooLong)
    -- the list's size at the limit, and one over: ":method: GET" is 3 + 7 + 32
  , fmap fst (decodeBlock 84 d0 (B.pack [0x82, 0x82])) === Right (Fields [(":method", "GET"), (":method", "GET")])
  , fmap fst (decodeBlock 83 d0 (B.pack [0x82, 0x82])) === Right ListTooLarge
  , fmap fst (decodeBlock 0 d0 B.empty) === Right (Fields [])
    -- an entry exactly the table's size fits, evicting all the others
  , fmap (tableEntries . decoderTable . snd)
      (decodeBlock maxBound (newDecoder 42 42) (B.concat (map encodeRep
        [ Literal WithIndexing (NameLiteral "k" False) "v" False
        , Literal WithIndexing (NameLiteral "k" False) "123456789" False ])))
      === Right [("k", "123456789")]
    -- = https://www.rfc-editor.org/rfc/rfc9113#section-4.3.1
    -- # An endpoint MUST treat a field block that follows an
    -- # acknowledgment of the reduction to the maximum dynamic table size as
    -- # a connection error (Section 5.4.1) of type COMPRESSION_ERROR if it
    -- # does not start with a conformant Dynamic Table Size Update
    -- # instruction.
  , decodeBlock maxBound (newDecoder 4096 100) (enc [Indexed 2]) === Left TableSizeUpdateMissing
  , decodeBlock maxBound (newDecoder 4096 100) B.empty === Left TableSizeUpdateMissing
  , fmap (tableMaxSize . decoderTable . snd) (decodeBlock maxBound (newDecoder 4096 100) (enc [SizeUpdate 100, Indexed 2]))
      === Right 100
  , fmap fst (decodeBlock maxBound (newDecoder 4096 100) (enc [SizeUpdate 0, SizeUpdate 100])) === Right (Fields [])
  , decodeBlock maxBound (setDecoderLimit 1000 d0) (enc [Indexed 2]) === Left TableSizeUpdateMissing
    -- a reduction, then an increase before any block: still owed, and
    -- to at most the smaller limit (RFC 7541 Section 4.2)
  , decodeBlock maxBound (setDecoderLimit 8000 (setDecoderLimit 100 d0)) (enc [Indexed 2])
      === Left TableSizeUpdateMissing
  , decodeBlock maxBound (setDecoderLimit 8000 (setDecoderLimit 100 d0)) (enc [SizeUpdate 8000])
      === Left TableSizeUpdateMissing
  , fmap fst (decodeBlock maxBound (setDecoderLimit 8000 (setDecoderLimit 100 d0)) (enc [SizeUpdate 100, SizeUpdate 8000]))
      === Right (Fields [])
  , decodeBlock maxBound (setDecoderLimit 1000 d0) (enc [SizeUpdate 1001]) === Left TableSizeUpdateMissing
    -- the update is owed once: the block after it needs none
  , fmap fst (decodeBlock maxBound (setDecoderLimit 1000 d0) (enc [SizeUpdate 1000]) >>= \(_, d) ->
      decodeBlock maxBound d (enc [Indexed 2])) === Right (Fields [(":method", "GET")])
    -- a limit above the table's size owes nothing, and the table keeps its size until updated
  , fmap fst (decodeBlock maxBound (setDecoderLimit 8000 d0) (enc [Indexed 2])) === Right (Fields [(":method", "GET")])
  , fmap (tableMaxSize . decoderTable . snd) (decodeBlock maxBound (newDecoder 4096 8192) (enc [Indexed 2]))
      === Right 4096
  , fmap (tableMaxSize . decoderTable . snd) (decodeBlock maxBound (newDecoder 4096 8192) (enc [SizeUpdate 8192]))
      === Right 8192
    -- the same entry twice is not an error
  , fmap (tableEntries . decoderTable . snd)
      (dec [ Literal WithIndexing (NameLiteral "k" False) "v" False
           , Literal WithIndexing (NameLiteral "k" False) "v" False ])
      === Right [("k", "v"), ("k", "v")]
    -- an entry larger than the table empties it
  , fmap (tableEntries . decoderTable . snd)
      (decodeBlock maxBound (newDecoder 40 40) (B.concat (map encodeRep
        [ Literal WithIndexing (NameLiteral "k" False) "v" False
        , Literal WithIndexing (NameLiteral "k" False) "much-too-long" False ])))
      === Right []
  ]
  where
    d0 = newDecoder 4096 4096
    enc = B.concat . map encodeRep
    dec = decodeBlock maxBound d0 . enc
