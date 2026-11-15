{-# LANGUAGE OverloadedStrings #-}
module SafePathTests (safePathTests) where

import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Hyperdrive.Semantics.SafePath
import Hyperdrive.Semantics.URI
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

safePathTests :: TestTree
safePathTests = testGroup "SafePath"
  [ testProperty "path traversal attacks are rejected"   prop_attacks
  , testProperty "legitimate paths are accepted"          prop_legitimate
  , testProperty "joined safe segments cannot escape (exhaustive)" prop_cannotEscape
  , testProperty "mkSafeSegment (exhaustive)"            prop_mkSafeSegment
  ]

-- | parse a path that is valid by construction
path :: ByteString -> Path
path bs = maybe (error ("invalid path: " ++ show bs)) id (mkPath bs)

safe :: ByteString -> Either UnsafeSegment [ByteString]
safe = fmap (map safeSegmentBytes) . safePathSegments . path

-- = https://www.rfc-editor.org/rfc/rfc9110#section-17.3
-- # For example, UNIX, Microsoft Windows, and other operating systems use
-- # ".." as a path component to indicate a directory level above the
-- # current one

-- = https://www.rfc-editor.org/rfc/rfc3986#section-7.3
-- # Note, however, that the "%00" percent-encoding
-- # (NUL) may require special handling and should be rejected if the
-- # application is not expecting to receive raw data within a component.

-- | Traversal payloads in the forms commonly used against web servers.
prop_attacks :: Property
prop_attacks = once $ conjoin
  [ counterexample (show p) (either (const True) (const False) (safe p) .&&. reason p === Just r)
  | (p, r) <-
      [ ("/..",                          DotSegment)
      , ("/static/../secret",            DotSegment)
      , ("/static/./secret/..",          DotSegment)
      , ("/static/%2e%2e/secret",        DotSegment)
      , ("/static/%2E%2E/secret",        DotSegment)
      , ("/static/.%2e/secret",          DotSegment)
      , ("/static/%2e./secret",          DotSegment)
      , ("/static/%2e",                  DotSegment)
      , ("/static/%2E%2E%2Fsecret",      ContainsSlash)
      , ("/static/..%2Fsecret",          ContainsSlash)
      , ("/static/%2F..%2Fsecret",       ContainsSlash)
      , ("/static/a%2F..%2F..%2Fetc%2Fpasswd", ContainsSlash)
      , ("/static/%2Fetc%2Fpasswd",      ContainsSlash)
      , ("/static/x%00.png",             ContainsNUL)
      , ("/static/%00",                  ContainsNUL)
      , ("/static/..%00",                ContainsNUL)
      ]
  ]
  where
    reason p = either Just (const Nothing) (safe p)

-- | Paths that look odd but are safe, and must not be rejected.
prop_legitimate :: Property
prop_legitimate = once $ conjoin
  [ counterexample (show p) (safe p === Right segs)
  | (p, segs) <-
      [ ("/",                             [])
      , ("",                              [])
      , ("/.well-known/acme-challenge/x", [".well-known", "acme-challenge", "x"])
      , ("/a//b/",                        ["a", "", "b", ""])
      , ("/...",                          ["..."])
      , ("/a..b/..c/d..",                 ["a..b", "..c", "d.."])
        -- decoded once only: a second decoding is the application's
        -- business, and would be a bug
      , ("/%252e%252e%252f",              ["%2e%2e%2f"])
        -- overlong UTF-8 for "..": these are not the bytes of ".."
      , ("/%c0%ae%c0%ae",                 ["\xc0\xae\xc0\xae"])
      , ("/a%20b/%E2%9C%93",              ["a b", "\xe2\x9c\x93"])
      ]
  ]

-- | For every path up to length 7 over an alphabet that can spell
-- ".", "..", "%2e", "%2E", "%2F" and "%00": whenever safePathSegments
-- accepts the path, joining the segments with "/" and splitting the
-- result again gives back exactly the same segments, none of which is
-- "." or ".." or contains NUL. So a file system sees exactly the
-- names we checked.
prop_cannotEscape :: Property
prop_cannotEscape = once $
  let paths    = [ bs | s <- concatMap (`replicateM` "/.%2eEF0") [0 .. 7]
                      , let bs = C.pack s, Just _ <- [mkPath bs] ]
      accepted = [ (p, segs) | p <- paths, Right segs <- [safe p] ]
      escapes (_, segs) =
        let joined = B.intercalate "/" segs
            parts  = B.split 0x2F joined
        in (not (null segs) && parts /= segs)
           || any (`elem` [".", ".."]) parts
           || B.elem 0x00 joined
      failures = filter escapes accepted
  in counterexample ("escapes: " ++ show (take 10 failures)) (null failures)
     .&&. counterexample "vacuous" (length accepted > 1000 && length accepted < length paths)

-- | mkSafeSegment on its own, for callers that decode segments some
-- other way: every string up to length 4 over an alphabet containing
-- the characters that matter
prop_mkSafeSegment :: Property
prop_mkSafeSegment = once $
  let check s =
        let bs = C.pack s
            expected
              | s `elem` [".", ".."] = Left DotSegment
              | '/' `elem` s         = Left ContainsSlash
              | '\0' `elem` s       = Left ContainsNUL
              | otherwise            = Right bs
        in fmap safeSegmentBytes (mkSafeSegment bs) == expected
      failures = filter (not . check) (concatMap (`replicateM` "./\0a") [0 .. 4])
  in counterexample ("failed for: " ++ show (take 10 failures)) (null failures)
