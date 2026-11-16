{-# LANGUAGE DeriveLift #-}
-- | The syntax of ABNF (RFC 5234), extended with case-sensitive
-- strings (RFC 7405) and comma-separated lists (RFC 9110,
-- Section 5.6.1).
--
-- The types follow the grammar of RFC 5234, Section 4, one type per
-- rule, so a grammar copied from an RFC is represented exactly as it
-- is written. Comments and layout are not kept. Constructs that the
-- RFCs define as equivalent are not merged, except where the
-- difference is only spelling: @3DIGIT@ and @3*3DIGIT@ are the same
-- 'Repeat', and @\"abc\"@ and @%i\"abc\"@ are the same 'CharVal'.
module Hyperdrive.ABNF.Types
  ( -- * Rule lists
    RuleList
  , Rule(..)
  , DefinedAs(..)
    -- * Rule names
  , RuleName
  , mkRuleName
  , ruleNameBytes
    -- * Elements
  , Alternation(..)
  , Concatenation(..)
  , Repetition(..)
  , Repeat(..)
  , RepeatKind(..)
  , Element(..)
    -- * Terminal values
  , Case(..)
  , Base(..)
  , NumValBody(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toLower)
import Data.List.NonEmpty (NonEmpty)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Natural (Natural)

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # rulelist       =  1*( rule / (*c-wsp c-nl) )

-- | The rules of a grammar, in the order they are written. A rule
-- list may define a name more than once: see 'Incremental'.
type RuleList = [Rule]

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # rule           =  rulename defined-as elements c-nl
data Rule = Rule
  { ruleName      :: RuleName
  , ruleDefinedAs :: DefinedAs
  , ruleElements  :: Alternation
  } deriving (Eq, Show, Lift)

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # defined-as     =  *c-wsp ("=" / "=/") *c-wsp
-- # ; basic rules definition and
-- # ;  incremental alternatives
data DefinedAs
  = Basic        -- ^ @=@
  | Incremental  -- ^ @=/@: adds alternatives to an earlier definition
  deriving (Eq, Ord, Show, Enum, Bounded, Lift)

-- | The name of a rule. Comparison ignores case, but the spelling it
-- was written with is kept for printing.
--
-- Invariant: the bytes match @rulename@.
newtype RuleName = RuleName ByteString
  deriving Lift

-- = https://www.rfc-editor.org/rfc/rfc5234#section-2.1
-- # Rule names are case insensitive.
instance Eq RuleName where
  a == b = foldedName a == foldedName b

instance Ord RuleName where
  compare a b = compare (foldedName a) (foldedName b)

instance Show RuleName where
  showsPrec d (RuleName bs) = showsPrec d bs

foldedName :: RuleName -> ByteString
foldedName (RuleName bs) = C.map toLower bs

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # rulename       =  ALPHA *(ALPHA / DIGIT / "-")
mkRuleName :: ByteString -> Maybe RuleName
mkRuleName bs = case C.uncons bs of
  Just (c, rest) | isAlpha c && C.all isNameChar rest -> Just (RuleName bs)
  _                                                    -> Nothing
  where
    isAlpha c    = isAsciiUpper c || isAsciiLower c
    isNameChar c = isAlpha c || isDigit c || c == '-'

-- | the name as it was written
ruleNameBytes :: RuleName -> ByteString
ruleNameBytes (RuleName bs) = bs

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # alternation    =  concatenation
-- # *(*c-wsp "/" *c-wsp concatenation)
newtype Alternation = Alternation (NonEmpty Concatenation)
  deriving (Eq, Show, Lift)

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # concatenation  =  repetition *(1*c-wsp repetition)
newtype Concatenation = Concatenation (NonEmpty Repetition)
  deriving (Eq, Show, Lift)

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # repetition     =  [repeat] element
data Repetition = Repetition (Maybe Repeat) Element
  deriving (Eq, Show, Lift)

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # repeat         =  1*DIGIT / (*DIGIT "*" *DIGIT)

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.6
-- # Default values are 0 and infinity

-- | @\<a\>*\<b\>@ or @\<n\>#\<m\>@. A missing minimum is 0 and a
-- missing maximum is infinity.
data Repeat = Repeat
  { repeatKind :: RepeatKind
  , repeatMin  :: Natural
  , repeatMax  :: Maybe Natural
  } deriving (Eq, Show, Lift)

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.7
-- # That is, exactly <n> occurrences of <element>.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1
-- # A construct "#" is defined, similar to "*", for defining comma-
-- # delimited lists of elements.  The full form is "<n>#<m>element"
-- # indicating at least <n> and at most <m> elements, each separated by a
-- # single comma (",") and optional whitespace (OWS, defined in
-- # Section 5.6.3).
data RepeatKind
  = Star  -- ^ @*@: RFC 5234 repetition
  | Hash  -- ^ @#@: an RFC 9110 comma-separated list
  deriving (Eq, Ord, Show, Enum, Bounded, Lift)

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # element        =  rulename / group / option /
-- # char-val / num-val / prose-val

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # group          =  "(" *c-wsp alternation *c-wsp ")"

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # option         =  "[" *c-wsp alternation *c-wsp "]"

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # num-val        =  "%" (bin-val / dec-val / hex-val)

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # prose-val      =  "<" *(%x20-3D / %x3F-7E) ">"
data Element
  = RuleRef RuleName
  | Group Alternation          -- ^ @( ... )@
  | Option Alternation         -- ^ @[ ... ]@
    -- | Invariant: the bytes are in @%x20-21 / %x23-7E@.
  | CharVal Case ByteString
  | NumVal Base NumValBody
    -- | Invariant: the bytes are in @%x20-3D / %x3F-7E@.
  | ProseVal ByteString
  deriving (Eq, Show, Lift)

-- = https://www.rfc-editor.org/rfc/rfc7405#section-2.2
-- # char-val       =  case-insensitive-string /
-- # case-sensitive-string

-- = https://www.rfc-editor.org/rfc/rfc7405#section-2.2
-- # case-insensitive-string =
-- # [ "%i" ] quoted-string

-- = https://www.rfc-editor.org/rfc/rfc7405#section-2.2
-- # case-sensitive-string =
-- # "%s" quoted-string

-- = https://www.rfc-editor.org/rfc/rfc7405#section-2.1
-- # To be consistent with prior implementations of ABNF, having no prefix
-- # means that the string is case insensitive and is equivalent to having
-- # the "%i" prefix.
data Case
  = CaseInsensitive  -- ^ @\"...\"@ or @%i\"...\"@
  | CaseSensitive    -- ^ @%s\"...\"@
  deriving (Eq, Ord, Show, Enum, Bounded, Lift)

-- | The base a value is written in. It does not change the meaning
-- of the value, but it is kept so a grammar prints as it was written.
data Base = Binary | Decimal | Hexadecimal
  deriving (Eq, Ord, Show, Enum, Bounded, Lift)

-- = https://www.rfc-editor.org/rfc/rfc5234#section-2.3
-- # In ABNF, a character is merely a non-negative integer.

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.4
-- # Concatenated numeric values and numeric value ranges cannot be
-- # specified in the same string.
data NumValBody
  = Values (NonEmpty Natural)  -- ^ @%x0D.0A@: a sequence of characters
  | Range Natural Natural      -- ^ @%x30-39@: one character in a range
  deriving (Eq, Show, Lift)
