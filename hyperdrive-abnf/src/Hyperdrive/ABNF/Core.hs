{-# LANGUAGE QuasiQuotes #-}
-- | The core rules of ABNF (RFC 5234, Appendix B.1), which grammars
-- use without defining: @ALPHA@, @DIGIT@, @CRLF@ and so on.
module Hyperdrive.ABNF.Core
  ( coreRules
  ) where

import Hyperdrive.ABNF.QQ (abnf)
import Hyperdrive.ABNF.Types (RuleList)

-- = https://www.rfc-editor.org/rfc/rfc5234#appendix-B.1
-- # Certain basic rules are in uppercase, such as SP, HTAB, CRLF, DIGIT,
-- # ALPHA, etc.

-- | The rules of Appendix B.1, copied from the RFC with their
-- comments. The tests check that this is what the RFC says.
coreRules :: RuleList
coreRules = [abnf|
         ALPHA          =  %x41-5A / %x61-7A   ; A-Z / a-z

         BIT            =  "0" / "1"

         CHAR           =  %x01-7F
                                ; any 7-bit US-ASCII character,
                                ;  excluding NUL

         CR             =  %x0D
                                ; carriage return

         CRLF           =  CR LF
                                ; Internet standard newline

         CTL            =  %x00-1F / %x7F
                                ; controls

         DIGIT          =  %x30-39
                                ; 0-9

         DQUOTE         =  %x22
                                ; " (Double Quote)

         HEXDIG         =  DIGIT / "A" / "B" / "C" / "D" / "E" / "F"

         HTAB           =  %x09
                                ; horizontal tab

         LF             =  %x0A
                                ; linefeed

         LWSP           =  *(WSP / CRLF WSP)
                                ; Use of this linear-white-space rule
                                ;  permits lines containing only white
                                ;  space that are no longer legal in
                                ;  mail headers and have caused
                                ;  interoperability problems in other
                                ;  contexts.
                                ; Do not use when defining mail
                                ;  headers and use with caution in
                                ;  other contexts.

         OCTET          =  %x00-FF
                                ; 8 bits of data

         SP             =  %x20

         VCHAR          =  %x21-7E
                                ; visible (printing) characters

         WSP            =  SP / HTAB
                                ; white space
  |]
