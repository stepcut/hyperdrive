{-# LANGUAGE OverloadedStrings #-}
-- | RFC 9218's parameters, read from field values (P2).
module PriorityTests (priorityTests) where

import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Hyperdrive.Semantics.Priority
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

priorityTests :: TestTree
priorityTests = testGroup "Priority (RFC 9218)"
  [ testProperty "parameters read from field values" (once parsing)
  , testProperty "parameters applied over a priority" (once applying)
  , testProperty "urgency only 0 to 7" (once urgencies)
  ]

params :: [ByteString] -> Maybe PriorityParams
params = parsePriorityParams

-- = https://www.rfc-editor.org/rfc/rfc9218#section-4
-- # Receivers parse the Dictionary as described in Section 4.2 of
-- # [STRUCTURED-FIELDS].  Where the Dictionary is successfully parsed,
-- # this document places the additional requirement that unknown priority
-- # parameters, priority parameters with out-of-range values, or values
-- # of unexpected types MUST be ignored.
parsing :: Property
parsing = conjoin
  [ params ["u=0"] === Just (PriorityParams (Just 0) Nothing)
  , params ["u=5, i"] === Just (PriorityParams (Just 5) (Just True))
  , params ["i=?0"] === Just (PriorityParams Nothing (Just False))
  , params [] === Just noParams
  , params ["u=1", "i"] === Just (PriorityParams (Just 1) (Just True))
    -- the last of a repeated key
  , params ["u=1, u=6"] === Just (PriorityParams (Just 6) Nothing)
    -- unknown parameters, out of range, unexpected types: ignored
  , params ["x=1, u=2"] === Just (PriorityParams (Just 2) Nothing)
  , params ["u=8, i"] === Just (PriorityParams Nothing (Just True))
  , params ["u=-1"] === Just noParams
  , params ["u=1.0"] === Just noParams
  , params ["u=\"1\""] === Just noParams
  , params ["u=(1 2)"] === Just noParams
  , params ["i=1"] === Just noParams
  , params ["i=a"] === Just noParams
    -- a parameter's own parameters do not matter
  , params ["u=2;x=1"] === Just (PriorityParams (Just 2) Nothing)
    -- not a Dictionary: not parsed at all
  , params ["u="] === Nothing
  , params ["U=1"] === Nothing
  , params ["u=1;"] === Nothing
  ]

-- = https://www.rfc-editor.org/rfc/rfc9218#section-8
-- # the intermediary might alter its understanding of the urgency from 5
-- # to 1, because it prefers the server-provided value over the client's.
-- # The incremental value continues to be true, i.e., the value specified
-- # by the client, as the server did not specify the incremental (i)
-- # parameter.
applying :: Property
applying = conjoin
  [ fmap (`applyParams` client) (params ["u=1"]) === mkPriority 1 True
  , applyParams noParams defaultPriority === defaultPriority
  , fmap (`applyParams` defaultPriority) (params ["i"]) === mkPriority 3 True
    -- an urgency out of range made by hand is ignored, as one in a field is
  , applyParams (PriorityParams (Just 9) Nothing) defaultPriority === defaultPriority
  , applyParams (PriorityParams (Just (-1)) Nothing) defaultPriority === defaultPriority
  ]
  where client = fromJust (mkPriority 5 True)

urgencies :: Property
urgencies = conjoin
  [ (priorityUrgency <$> mkPriority u False) === (if u >= 0 && u <= 7 then Just u else Nothing)
  | u <- [-2 .. 9] ]
  .&&. priorityUrgency defaultPriority === 3
  .&&. priorityIncremental defaultPriority === False
