{-# LANGUAGE OverloadedStrings #-}
-- | Extensible priorities (RFC 9218): the urgency and incremental
-- parameters, read from the @Priority@ field or a PRIORITY_UPDATE
-- frame's value (docs/priorities-design.md, P2). Shared by HTTP/2 and
-- HTTP/3.
module Hyperdrive.Semantics.Priority
  ( Priority
  , priorityUrgency
  , priorityIncremental
  , mkPriority
  , defaultPriority
  , PriorityParams(..)
  , noParams
  , parsePriorityParams
  , applyParams
  ) where

import Data.ByteString (ByteString)
import Hyperdrive.Semantics.StructuredField

-- | A response's priority: urgency 0 (first) to 7 (last), and whether
-- the client can use it in parts. Made only with 'mkPriority', so the
-- urgency is always in range.
data Priority = Priority !Int !Bool
  deriving (Eq, Ord, Show)

priorityUrgency :: Priority -> Int
priorityUrgency (Priority u _) = u

priorityIncremental :: Priority -> Bool
priorityIncremental (Priority _ i) = i

mkPriority :: Int -> Bool -> Maybe Priority
mkPriority u i
  | u >= 0 && u <= 7 = Just (Priority u i)
  | otherwise = Nothing

-- = https://www.rfc-editor.org/rfc/rfc9218#section-4.1
-- # The urgency (u) parameter value is Integer (see Section 3.3.1 of
-- # [STRUCTURED-FIELDS]), between 0 and 7 inclusive, in descending order
-- # of priority.  The default is 3.
--
-- = https://www.rfc-editor.org/rfc/rfc9218#section-4.2
-- # The default value of the incremental parameter is false (0).
defaultPriority :: Priority
defaultPriority = Priority 3 False

-- | The parameters a field value names; those it does not name are
-- 'Nothing'. What that means differs: defaults in a request and in
-- PRIORITY_UPDATE (Sections 4 and 7), the client's values kept in a
-- response (Section 8).
data PriorityParams = PriorityParams
  { paramUrgency     :: !(Maybe Int)
  , paramIncremental :: !(Maybe Bool)
  } deriving (Eq, Show)

noParams :: PriorityParams
noParams = PriorityParams Nothing Nothing

-- | The parameters of a @Priority@ field's lines, or of a PRIORITY_UPDATE
-- frame's value (one line); 'Nothing' if they are not a Dictionary, and
-- then the field is as if absent (RFC 9651, Section 4.2).
--
-- = https://www.rfc-editor.org/rfc/rfc9218#section-4
-- # Receivers parse the Dictionary as described in Section 4.2 of
-- # [STRUCTURED-FIELDS].  Where the Dictionary is successfully parsed,
-- # this document places the additional requirement that unknown priority
-- # parameters, priority parameters with out-of-range values, or values
-- # of unexpected types MUST be ignored.
parsePriorityParams :: [ByteString] -> Maybe PriorityParams
parsePriorityParams fieldLines = do
  dict <- parseDictionary (combineFieldLines fieldLines)
  pure PriorityParams
    { paramUrgency = case dictionaryMember "u" dict of
        Just (MemberItem (Item (SFInteger u) _)) | u >= 0 && u <= 7 -> Just (fromInteger u)
        _ -> Nothing
    , paramIncremental = case dictionaryMember "i" dict of
        Just (MemberItem (Item (SFBoolean i) _)) -> Just i
        _ -> Nothing
    }

-- | The parameters named replace those of the priority given; the rest
-- are kept, and so is the urgency if one named is out of range. Over
-- 'defaultPriority', a request's or a PRIORITY_UPDATE's priority:
--
-- = https://www.rfc-editor.org/rfc/rfc9218#section-4
-- # When receiving an HTTP request that does not carry these
-- # priority parameters, a server SHOULD act as if their default values
-- # were specified.
--
-- = https://www.rfc-editor.org/rfc/rfc9218#section-7
-- # A PRIORITY_UPDATE frame communicates a complete set of all priority
-- # parameters in the Priority Field Value field.  Omitting a priority
-- # parameter is a signal to use its default value.
--
-- Over the client's, a response's (P4):
--
-- = https://www.rfc-editor.org/rfc/rfc9218#section-8
-- # The absence of a priority parameter in an HTTP response indicates the
-- # server's disinterest in changing the client-provided value.
applyParams :: PriorityParams -> Priority -> Priority
applyParams (PriorityParams u i) (Priority u0 i0) =
  Priority (maybe u0 priorityUrgency (u >>= \u' -> mkPriority u' i0)) (maybe i0 id i)
