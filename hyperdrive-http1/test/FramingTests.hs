{-# LANGUAGE OverloadedStrings, PatternSynonyms, QuasiQuotes #-}
module FramingTests (framingTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Hyperdrive.ABNF.Grammar (mkGrammar)
import Hyperdrive.ABNF.HTTP (rfc3986, rfc9110, rfc9110Errata, withErrata)
import Hyperdrive.ABNF.QQ (abnf)
import Hyperdrive.ABNF.Recognize (ListForm(..), library, matches)
import Hyperdrive.ABNF.Types (mkRuleName)
import Hyperdrive.HTTP1.Framing
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Message (HttpVersion, pattern HTTP_1_0, pattern HTTP_1_1)
import Hyperdrive.Semantics.Status (pattern BadRequest, pattern NotImplemented)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

framingTests :: TestTree
framingTests = testGroup "Request framing (RFC 9112 Section 6)"
  [ testProperty "the labelled samples agree with RFC 9110's grammars" prop_labels
  , testProperty "every combination of up to two lines of each field (exhaustive)" prop_exhaustive
  , testProperty "the RFC's examples, and the status for each error" prop_examples
  ]

------------------------------------------------------------------------------
-- labelled samples
------------------------------------------------------------------------------

-- | Content-Length values, each with the numbers it lists, or Nothing if
-- it is not a list of decimal numbers. A value is read as a list
-- (RFC 9112 Section 6.3, rule 5), which may have empty elements.
contentLengths :: [(ByteString, Maybe [Integer])]
contentLengths =
  [ ("0", Just [0]), ("5", Just [5]), ("05", Just [5]), ("5, 5", Just [5, 5]), ("5,5", Just [5, 5])
  , ("5, 05", Just [5, 5]), ("5, 6", Just [5, 6]), (", 5", Just [5]), ("", Just [])
  , ("18446744073709551615", Just [18446744073709551615])
  , ("18446744073709551616", Just [18446744073709551616])
  , ("-1", Nothing), ("+5", Nothing), ("5.0", Nothing), ("0x10", Nothing), ("5 5", Nothing)
  , ("5;q=1", Nothing), ("\"5\"", Nothing) ]

-- | Transfer-Encoding values, each with its codings (the name in lower
-- case, and whether it has parameters), or Nothing if it is not a list
-- of transfer codings.
transferEncodings :: [(ByteString, Maybe [(ByteString, Bool)])]
transferEncodings =
  [ ("chunked", Just [("chunked", False)]), ("Chunked", Just [("chunked", False)])
  , ("CHUNKED", Just [("chunked", False)])
  , ("gzip, chunked", Just [("gzip", False), ("chunked", False)])
  , ("chunked, gzip", Just [("chunked", False), ("gzip", False)])
  , ("chunked, chunked", Just [("chunked", False), ("chunked", False)])
  , ("chunked;x=y", Just [("chunked", True)]), ("chunked ; x = y", Just [("chunked", True)])
  , ("gzip;q=\"a,b\", chunked", Just [("gzip", True), ("chunked", False)])
  , ("identity", Just [("identity", False)]), ("", Just [])
  , (",chunked,", Just [("chunked", False)]), ("x-unknown, chunked", Just [("x-unknown", False), ("chunked", False)])
  , ("chunked x", Nothing), ("chunked;", Nothing), ("chunked;x", Nothing), ("chunked=1", Nothing)
  , ("\"chunked\"", Nothing), ("chunked;x=", Nothing) ]

-- | RFC 9110's grammars, in the form recipients accept lists:
-- @#Content-Length@ and @#transfer-coding@.
grammarMatches :: String -> ByteString -> Bool
grammarMatches r = either (error . show) id . matches Recipient lib "L" (fromJust (mkRuleName (C.pack r)))
  where
    lib = library
      [ ("L", grammar [abnf|
          lengths  = #length
          length   = <Content-Length, see [HTTP], Section 8.6>
          codings  = #coding
          coding   = <transfer-coding, see [HTTP], Section 10.1.4>
          |])
      , ("HTTP", grammar (withErrata rfc9110Errata rfc9110)), ("URI", grammar rfc3986) ]
    grammar = either (error . show) id . mkGrammar

-- = https://www.rfc-editor.org/rfc/rfc9110#section-8.6
-- # Content-Length = 1*DIGIT

-- = https://www.rfc-editor.org/rfc/rfc9110#section-10.1.4
-- # transfer-coding    = token *( OWS ";" OWS transfer-parameter )
-- # transfer-parameter = token BWS "=" BWS ( token / quoted-string )
prop_labels :: Property
prop_labels = once $ conjoin $
  [ counterexample (show v) $ grammarMatches "lengths" v === (lab /= Nothing) | (v, lab) <- contentLengths ]
  ++ [ counterexample (show v) $ grammarMatches "codings" v === (lab /= Nothing) | (v, lab) <- transferEncodings ]

------------------------------------------------------------------------------
-- the oracle
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9112#section-6.3
-- # The length of a message body is determined by one of the following
-- # (in order of precedence):

-- | RFC 9112's rules, in order, applied to the labels of the field lines.
expected :: HttpVersion -> [Maybe [Integer]] -> [Maybe [(ByteString, Bool)]] -> Either FramingError RequestFraming
expected version cls tes
  | not (null tes), version == HTTP_1_0 = Left TransferEncodingInHTTP10
  | not (null tes), not (null cls)      = Left BothLengthAndEncoding
  | not (null tes) = case sequence tes of
      Nothing -> Left InvalidTransferEncoding
      Just lists -> case reverse (concat lists) of
        ("chunked", True) : _ -> Left ChunkedWithParameters
        ("chunked", False) : earlier
          | "chunked" `elem` map fst earlier -> Left ChunkedTwice
          | (name, _) : _ <- reverse earlier -> Left (UnsupportedTransferCoding name)
          | otherwise                        -> Right Chunked
        _ -> Left ChunkedNotFinal
  | not (null cls) = case concat <$> sequence cls of
      Just (n : ns) | all (== n) ns, n < 2 ^ (64 :: Int) -> Right (ContentLength (fromInteger n))
      _ -> Left InvalidContentLength
  | otherwise = Right NoBody

-- | Every combination, for both versions, of none, one or two field
-- lines of each of Content-Length and Transfer-Encoding, drawn from the
-- samples: over 200,000 requests.
prop_exhaustive :: Property
prop_exhaustive = once $
  let upToTwo xs = [[]] ++ [ [x] | x <- xs ] ++ [ [x, y] | x <- xs, y <- xs ]
      cases = [ (v, cl, te) | v <- [HTTP_1_0, HTTP_1_1], cl <- upToTwo contentLengths, te <- upToTwo transferEncodings ]
      request cl te = fieldsFromList ([ (hContentLength, value v) | (v, _) <- cl ] ++ [ (hTransferEncoding, value v) | (v, _) <- te ])
      failures = [ (v, map fst cl, map fst te, got, want)
                 | (v, cl, te) <- cases
                 , let got = requestFraming v (request cl te)
                       want = expected v (map snd cl) (map snd te)
                 , got /= want ]
  in counterexample ("disagreements (version, Content-Length, Transfer-Encoding, got, expected): " ++ show (take 5 failures)) $
       length cases > 200000 .&&. null failures

value :: ByteString -> FieldValue
value = either (error . show) id . mkFieldValue

------------------------------------------------------------------------------
-- examples
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9112#section-6.1
-- # For example,
-- #
-- # Transfer-Encoding: gzip, chunked
-- #
-- # indicates that the content has been compressed using the gzip coding
-- # and then chunked using the chunked coding while forming the message
-- # body.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-6.1
-- # A server that receives a request message with a transfer coding it
-- # does not understand SHOULD respond with 501 (Not Implemented).
prop_examples :: Property
prop_examples = once $ conjoin
  [ framing HTTP_1_1 [] [] === Right NoBody
  , framing HTTP_1_0 [] [] === Right NoBody
  , framing HTTP_1_1 ["42"] [] === Right (ContentLength 42)
  , framing HTTP_1_1 ["42", "42"] [] === Right (ContentLength 42)
  , framing HTTP_1_1 ["42", "43"] [] === Left InvalidContentLength
  , framing HTTP_1_1 [] ["chunked"] === Right Chunked
  , framing HTTP_1_1 [] ["gzip", "chunked"] === Left (UnsupportedTransferCoding "gzip")
  , framing HTTP_1_1 [] ["gzip, chunked"] === Left (UnsupportedTransferCoding "gzip")
  , framing HTTP_1_1 ["5"] ["chunked"] === Left BothLengthAndEncoding
  , framing HTTP_1_0 ["5"] ["chunked"] === Left TransferEncodingInHTTP10
  , framing HTTP_1_1 [] ["chunked, gzip"] === Left ChunkedNotFinal
  , map framingErrorStatus [ TransferEncodingInHTTP10, BothLengthAndEncoding, InvalidTransferEncoding
                           , ChunkedNotFinal, ChunkedTwice, ChunkedWithParameters, InvalidContentLength ]
      === replicate 7 BadRequest
  , framingErrorStatus (UnsupportedTransferCoding "gzip") === NotImplemented
  ]
  where
    framing v cls tes = requestFraming v (fieldsFromList ([ (hContentLength, value c) | c <- cls ] ++ [ (hTransferEncoding, value t) | t <- tes ]))
