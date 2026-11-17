{-# LANGUAGE OverloadedStrings #-}
-- | RFC 9110's list construct, @\<n\>#\<m\>element@, rewritten into
-- plain ABNF.
--
-- RFC 9110 defines two expansions. The one for senders, which allows
-- no empty elements, can be written in ABNF, and is the one RFC 9110
-- and RFC 9112 use in their collected ABNF; 'senderForm' produces it.
-- The one for recipients cannot, because empty elements do not count
-- towards the bounds; "Hyperdrive.ABNF.Recognize" interprets it
-- directly.
module Hyperdrive.ABNF.Lists
  ( senderForm
  , senderFormRule
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust)
import Hyperdrive.ABNF.Types

senderFormRule :: Rule -> Rule
senderFormRule (Rule n d a) = Rule n d (senderForm a)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1.1
-- # 1#element => element *( OWS "," OWS element )

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1.1
-- # #element => [ 1#element ]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1.1
-- # <n>#<m>element => element <n-1>*<m-1>( OWS "," OWS element )

-- | Every @#@ list rewritten with the expansions above. The two parts of
-- @element *( OWS \",\" OWS element )@ become two items of the
-- concatenation the list was in, and @[ ... ]@ is an option.
senderForm :: Alternation -> Alternation
senderForm (Alternation cs) = Alternation (fmap concatenation cs)
  where
    concatenation (Concatenation rs) = Concatenation (concatMapNE repetition rs)

    repetition (Repetition (Just (Repeat Hash lo hi)) e) = list lo hi (element e)
    repetition (Repetition r e) = Repetition r (element e) :| []

    element (Group a)  = Group (senderForm a)
    element (Option a) = Option (senderForm a)
    element e          = e

    list 0 hi e = Repetition Nothing (Option (Alternation (Concatenation (list 1 hi e) :| []))) :| []
    -- with at most one element, there is nothing to separate
    list _ (Just 1) e = Repetition Nothing e :| []
    list lo hi e =
      Repetition Nothing e :|
        [ Repetition (Just (Repeat Star (lo - 1) (subtract 1 <$> hi))) $ Group $ Alternation $
            Concatenation (ref "OWS" :| [Repetition Nothing (CharVal CaseInsensitive ","), ref "OWS", Repetition Nothing e]) :| [] ]

    ref = Repetition Nothing . RuleRef . fromJust . mkRuleName

    concatMapNE f (x :| xs) = let (y :| ys) = f x in y :| (ys ++ concatMap (toList' . f) xs)
    toList' (y :| ys) = y : ys
