{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module Hyperdrive.Parser.ABNF.Parser where

import ABNF.ClassyParser.Classes
import ABNF.ClassyParser.Gen.Attoparsec             (GenAttoparsec(..))
import ABNF.ClassyParser.Gen.ABNF                   (GenABNF(..), normalizeAlternation)
import ABNF.Types                (Rule(..), RuleName(..), RuleList(..), RuleMap(..), Alternation(..), ruleMap)
import ABNF.Parser               (abnf,abnfRule)
import ABNF.Printer              (ppElements, ppRuleList)
import ABNF.CoreRules            (core_ruleList)
import Data.CaseInsensitive      (CI, mk, original)
import Control.Applicative       (Applicative(pure), (<$>))
import Control.Monad.Reader      (Reader(..), runReader)
import Data.Maybe                (fromMaybe)
import Data.ByteString           (ByteString, pack)
import Data.Text                 (Text)
import qualified Data.Map        as Map
import Data.Maybe                (fromJust)
import Data.Monoid               ((<>), mempty)
import Hyperdrive.Types          (Request(..), RequestRaw(..), RequestBodyLength(..))
import Instances.TH.Lift         ()
import Language.Haskell.TH.Syntax (Lift(lift))
import Language.Haskell.TH.Lift  (deriveLift)
import Network.HTTP.Types        (ByteRange(..), HttpVersion(..), Method(..), Query(..), StdMethod(..), Header, HeaderName(..), RequestHeaders)
import Network.Socket            (SockAddr(..), PortNumber(..))

deriveLift ''ByteRange
deriveLift ''RequestBodyLength
deriveLift ''SockAddr
deriveLift ''PortNumber

------------------------------------------------------------------------------
-- basic parsers
------------------------------------------------------------------------------

sp_rule :: Rule
sp_rule = [abnfRule|SP             =  %x20|]

sp_parser :: ClassyParser repr => repr Char
sp_parser = pHexChar "20"

crlf_rule :: Rule
crlf_rule = [abnfRule|CRLF = CR LF |]

crlf_parser :: (ClassyParser repr) => repr ()
crlf_parser = pHexChar "0D" `appR` pHexChar "0A" `appR` pureR ()

separators = "()<>@,;:\\\"/[]?={} \t"
ctl = '\127':['\0'..'\31']

token_parser :: (ClassyParser repr) => repr ByteString
token_parser =
    pTakeWhile1 (NotInClass (separators++ctl))

fieldName_rule :: Rule
fieldName_rule = [abnfRule|field-name     = token|]

fieldName_parser :: (ClassyParser repr) => repr ByteString
fieldName_parser = token_parser

fieldValue_parser :: (ClassyParser repr) => repr ByteString
fieldValue_parser =
    pTakeWhile1 (NotInClass ctl)


------------------------------------------------------------------------------
-- StdMethod
------------------------------------------------------------------------------

deriveLift ''StdMethod

std_method_parser :: ClassyParser repr => repr StdMethod
std_method_parser =
    pEnumerate [ ("OPTIONS", OPTIONS)
               , ("GET"    , GET)
               , ("HEAD"   , HEAD)
               , ("POST"   , POST)
               , ("PUT"    , PUT)
               , ("DELETE" , DELETE)
               , ("TRACE"  , TRACE)
               , ("CONNECT", CONNECT)
               , ("PATCH"  , PATCH)
               ]

method_parser :: ClassyParser repr => repr ByteString
method_parser =
    pEnumerate [ ("OPTIONS", "OPTIONS")
               , ("GET"    , "GET")
               , ("HEAD"   , "HEAD")
               , ("POST"   , "POST")
               , ("PUT"    , "PUT")
               , ("DELETE" , "DELETE")
               , ("TRACE"  , "TRACE")
               , ("CONNECT", "CONNECT")
               , ("PATCH"  , "PATCH")
               ]


uri_parser :: ClassyParser repr => repr ByteString
uri_parser = pCharVal "/"

------------------------------------------------------------------------------
-- HttpVersion
------------------------------------------------------------------------------

deriveLift ''HttpVersion

class HttpVersionC repr where
    httpVersion :: repr (Int -> Int -> HttpVersion)

instance HttpVersionC GenAttoparsec where
    httpVersion = GA [| pure HttpVersion |]

instance HttpVersionC GenABNF where
    httpVersion = GenABNF (Alternation [])

httpVersion_rule :: Rule
httpVersion_rule =
    [abnfRule|HTTP-Version   = "HTTP" "/" 1*DIGIT "." 1*DIGIT |]

httpVersion_parser :: (ClassyParser repr, HttpVersionC repr) => repr HttpVersion
httpVersion_parser =
    (pCharVal "HTTP")  `appR` (pCharVal "/") `appR` (httpVersion `app` (digitsToInt (pMany1 pDigit)) `appL` pCharVal "." `app` (digitsToInt (pMany1 pDigit)))

------------------------------------------------------------------------------
-- Header
------------------------------------------------------------------------------

class CiC repr where
    mkCI :: repr (ByteString -> CI ByteString)

instance CiC GenAttoparsec where
    mkCI = GA [| pure mk |]

instance CiC GenABNF where
    mkCI = GenABNF (Alternation [])

instance (Lift s) => Lift (CI s) where
    lift ci =
        let orig = original ci
        in [| mk orig |]

class NothingIsEmpty repr where
    nothingIsEmpty :: repr (Maybe ByteString -> ByteString)

instance NothingIsEmpty GenABNF where
    nothingIsEmpty = GenABNF (Alternation [])

instance NothingIsEmpty GenAttoparsec where
    nothingIsEmpty = GA [| pure (fromMaybe mempty) |]

messageHeader_Rule :: Rule
messageHeader_Rule = [abnfRule|message-header = field-name ":" [ field-value ] |]

messageHeader_parser :: (ClassyParser repr, CiC repr, NothingIsEmpty repr) => repr Header
messageHeader_parser =
    pair `app` (mkCI `app` fieldName_parser) `appL` pCharVal ":" `app` (nothingIsEmpty `app` (pOptional $ fieldValue_parser))

messageHeaders_parser :: (ClassyParser repr, CiC repr, NothingIsEmpty repr) => repr [Header]
messageHeaders_parser =
    pMany (messageHeader_parser  `appL` crlf_parser)


requestLine_rule = [abnfRule|Request-Line   = Method SP Request-URI SP HTTP-Version CRLF|]

requestLine =
    [abnf|
Request-Line   = Method SP Request-URI SP HTTP-Version CRLF

HTTP-Version   = "HTTP" "/" 1*DIGIT "." 1*DIGIT

Method         = "OPTIONS"                ; Section 9.2
               | "GET"                    ; Section 9.3
               | "HEAD"                   ; Section 9.4
               | "POST"                   ; Section 9.5
               | "PUT"                    ; Section 9.6
               | "DELETE"                 ; Section 9.7
               | "TRACE"                  ; Section 9.8
               | "CONNECT"                ; Section 9.9

CRLF = CR LF
         |]

{-
Request       = Request-Line              ; Section 5.1
                  *(( general-header        ; Section 4.5
                    | request-header         ; Section 5.3
                    | entity-header ) CRLF)

-}
{-
data Request = Request
    { method  :: StdMethod
    , uri     :: ByteString
    , version :: HttpVersion
    , headers :: RequestHeaders
    }
    deriving Show
-- $(deriveLift ''Request)
-}
class (HttpVersionC repr) => RequestC repr where
    request :: repr (   Bool
                     -> Method
                     -> ByteString
                     -> ByteString
                     -> HttpVersion
                     -> RequestHeaders
                     -> SockAddr
                     -> [Text]
                     -> Query
                     -> [(Text, Text)]
                     -> RequestBodyLength
                     -> ByteString
                     -> Maybe ByteRange
                     -> Request)

instance RequestC GenAttoparsec where
    request = GA [| pure Request |]

instance RequestC GenABNF where
    request = GenABNF (Alternation [])

class (HttpVersionC repr) => RequestRawC repr where
    requestRaw :: repr (   Method
                        -> ByteString
                        -> HttpVersion
                        -> RequestHeaders
                        -> RequestRaw)

instance RequestRawC GenAttoparsec where
    requestRaw = GA [| pure RequestRaw |]

instance RequestRawC GenABNF where
    requestRaw = GenABNF (Alternation [])

request_raw_parser :: (ClassyParser repr, RequestRawC repr, CiC repr, NothingIsEmpty repr) => repr RequestRaw
request_raw_parser =
  requestRaw `app` method_parser         `appL` sp_parser
             `app` uri_parser            `appL` sp_parser
             `app` httpVersion_parser    `appL` crlf_parser
             `app` messageHeaders_parser `appL` crlf_parser

request_parser :: (ClassyParser repr, RequestC repr, CiC repr, NothingIsEmpty repr) => repr Request
request_parser =
  request `app` (pureR False)             -- FIXME: we don't want to have to default this
          `app` method_parser         `appL` sp_parser
          `app` uri_parser            `appL` sp_parser
          `app` (pureR mempty)
          `app` httpVersion_parser    `appL` crlf_parser
          `app` messageHeaders_parser `appL` crlf_parser
          `app` (pureR (SockAddrUnix "")) -- FIXME -- we don't want to have to default this
          `app` (pureR [])
          `app` (pureR [])
          `app` (pureR [])
          `app` (pureR ChunkedBody)
          `app` (pureR "")
          `app` (pureR Nothing)

  {-
    request `app` method_parser         `appL` sp_parser
            `app` uri_parser            `appL` sp_parser
            `app` httpVersion_parser    `appL` crlf_parser
            `app` messageHeaders_parser `appL` crlf_parser


testGen =
    do {- print $ ppElements $ runGenABNF $  pCharVal "foo"
       print $ ppElements $ runGenABNF $ method_parser
       print $ ppElements $ runGenABNF $ uri_parser
       print $ ppElements $ runGenABNF $ httpVersion_parser
       print $ ppElements $ runGenABNF $ messageHeaders_parser -}
       print $ ppElements $ runGenABNF $ request_parser

-- check :: RuleMap -> Rule -> p -> IO ()
check :: RuleMap -> Rule -> GenABNF a -> IO ()
check rulemap rule parser =
    do let (Rule _ alternation) = rule
           method_n   = runReader (normalizeAlternation alternation)         rulemap
           method_gen = runReader (normalizeAlternation (runGenABNF parser)) rulemap
       putStrLn "normalized rules from spec"
       print $ ppElements $ method_n
--       print $ method_n
       putStrLn "normalized generated rules"
       print $ ppElements $ method_gen
       print (method_n == method_gen)

-- test_method = check Map.empty method_rule method_parser
{-
test_request =
    check (ruleMap (core_ruleList <> request_rules))
          (Rule (RuleName "Request") (fromJust $ Map.lookup (RuleName "Request") (ruleMap request_rules)))
          request_parser
-}
--    check requestLine requestLine_rule


test_rule rulelist rulename parser =
    case Map.lookup (RuleName rulename) (ruleMap rulelist) of
      Nothing -> error $ "rulename " ++ show rulename ++ "not found."
      (Just elements) ->
          check (ruleMap (rulelist <> core_ruleList))
                (Rule (RuleName rulename) elements)
                parser

test_method         = test_rule request_rules "Method"         method_parser
test_request_uri    = test_rule request_rules "Request-URI"    uri_parser
test_httpVersion    = test_rule request_rules "HTTP-Version"   httpVersion_parser
test_fieldName      = test_rule request_rules "field-name"     fieldName_parser
test_fieldValue     = test_rule request_rules "field-value"    fieldValue_parser
test_messageHeader  = test_rule request_rules "message-header" messageHeader_parser
test_request        = test_rule request_rules "Request"        request_parser

------------------------------------------------------------------------------
-- ABNF Rules required to parse a Request
------------------------------------------------------------------------------

request_rules :: RuleList
request_rules =
    [abnf|
Request       = Request-Line              ; Section 5.1
                *(message-header CRLF)
                CRLF

Request-Line   = Method SP Request-URI SP HTTP-Version CRLF

Request-URI    = "/"

HTTP-Version   = "HTTP" "/" 1*DIGIT "." 1*DIGIT

Method         = "OPTIONS"                ; Section 9.2
               | "GET"                    ; Section 9.3
               | "HEAD"                   ; Section 9.4
               | "POST"                   ; Section 9.5
               | "PUT"                    ; Section 9.6
               | "DELETE"                 ; Section 9.7
               | "TRACE"                  ; Section 9.8
               | "CONNECT"                ; Section 9.9
               | "PATCH"                  ; hyperdrive-extension

CRLF = CR LF

message-header = field-name ":" [ field-value ]

field-name     = token

token = 1*("!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." / "0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9" / "A" / "B" / "C" / "D" / "E" / "F" / "G" / "H" / "I" / "J" / "K" / "L" / "M" / "N" / "O" / "P" / "Q" / "R" / "S" / "T" / "U" / "V" / "W" / "X" / "Y" / "Z" / "^" / "_" / "`" / "a" / "b" / "c" / "d" / "e" / "f" / "g" / "h" / "i" / "j" / "k" / "l" / "m" / "n" / "o" / "p" / "q" / "r" / "s" / "t" / "u" / "v" / "w" / "x" / "y" / "z" / "|" / "~")

field-value = 1*(" " / "!" / %x22 / "#" / "$" / "%" / "&" / "'" / "(" / ")" / "*" / "+" / "," / "-" / "." / "/" / "0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9" / ":" / ";" / "<" / "=" / ">" / "?" / "@" / "A" / "B" / "C" / "D" / "E" / "F" / "G" / "H" / "I" / "J" / "K" / "L" / "M" / "N" / "O" / "P" / "Q" / "R" / "S" / "T" / "U" / "V" / "W" / "X" / "Y" / "Z" / "[" / "\" / "]" / "^" / "_" / "`" / "a" / "b" / "c" / "d" / "e" / "f" / "g" / "h" / "i" / "j" / "k" / "l" / "m" / "n" / "o" / "p" / "q" / "r" / "s" / "t" / "u" / "v" / "w" / "x" / "y" / "z" / "{" / "|" / "}" / "~")

         |]
-}
