{-# LANGUAGE OverloadedStrings #-}
-- | Getting ABNF out of the plain-text form of an RFC.
--
-- RFC 5234 notes that a grammar printed in a specification "might
-- need preprocessing to ensure that it can be interpreted by an ABNF
-- parser". In the plain-text RFCs that means removing page breaks,
-- removing the indentation of the whole block, and using CRLF line
-- endings.
module Hyperdrive.ABNF.RFC
  ( unpaginate
  , section
  , sectionABNF
  , abnfBlocks
  , dedent
  , toCRLF
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # This syntax requires a formatting of rules that is relatively
-- # strict.  Hence, the version of a ruleset included in a
-- # specification might need preprocessing to ensure that it can
-- # be interpreted by an ABNF parser.

-- | Split an RFC into lines, removing each page's footer, the form
-- feed and the next page's header, and the blank lines around them,
-- so that a rule broken across pages is joined up again. A UTF-8
-- byte order mark at the start is removed too.
unpaginate :: ByteString -> [ByteString]
unpaginate = go . map (C.dropWhileEnd (== '\r')) . C.lines . stripBOM
  where
    stripBOM bs = maybe bs id (B.stripPrefix "\xEF\xBB\xBF" bs)
    go ls = case break isBreak ls of
      (before, _ : after) ->
        let before' = dropBlankEnd (dropLast before)  -- the footer
            after'  = dropWhile isBlank (drop 1 after) -- the header
        in before' ++ go after'
      (before, []) -> before
    isBreak l = "\f" `B.isPrefixOf` l
    dropLast = reverse . drop 1 . reverse
    dropBlankEnd = reverse . dropWhile isBlank . reverse

-- | The lines of the section whose heading starts with the given text
-- (for example @\"4.  \"@ or @\"Appendix A.\"@), up to the next
-- heading. Headings are the lines that are not indented.
section :: ByteString -> [ByteString] -> [ByteString]
section heading ls =
  case dropWhile (not . (heading `B.isPrefixOf`)) ls of
    _ : rest -> takeWhile (\l -> isBlank l || isIndented l) rest
    []       -> []
  where isIndented l = " " `B.isPrefixOf` l

-- | The ABNF in a section, ready for the parser: the lines from the
-- first one that starts a rule (a rule name followed by @=@) to the
-- end of the section, without their indentation, with CRLF line
-- endings. The prose before the first rule is dropped.
sectionABNF :: ByteString -> [ByteString] -> ByteString
sectionABNF heading = toCRLF . dedent . dropWhile (not . startsRule) . section heading

-- | Does the line start with a rule name followed by @=@?
startsRule :: ByteString -> Bool
startsRule l =
  let (name, rest) = C.span (\c -> isAsciiUpper c || isAsciiLower c || isDigit c || c == '-') (C.dropWhile (== ' ') l)
  in not (B.null name) && "=" `B.isPrefixOf` C.dropWhile (== ' ') rest

-- | Every paragraph of the RFC that starts like a rule (a rule name
-- followed by @=@), up to the first line indented less than it, with
-- the number of the section it is in. A rule that follows prose
-- without a blank line between them starts a paragraph, since that
-- only happens where a page break was removed. (@5.6.2@,
-- or @A@ for Appendix A), ready for the parser. Examples that happen to
-- look like rules are included; they will not parse.
abnfBlocks :: [ByteString] -> [(ByteString, ByteString)]
abnfBlocks = go "" []
  where
    go sec cur (l : ls)
      | isBlank l = flush sec cur (go sec [] ls)
      | Just n <- heading l = flush sec cur (go n [] ls)
      -- a line indented less than the paragraph's first one is prose
      -- that follows it: unpaginate removes the blank lines around a
      -- page break, which can join the two
      | (first : _) <- reverse cur, indent l < indent first = flush sec cur (go sec [l] ls)
      -- and a rule after prose starts a paragraph of its own, for the
      -- same reason
      | (first : _) <- reverse cur, startsRule l, not (startsRule first) = flush sec cur (go sec [l] ls)
      | otherwise = go sec (l : cur) ls
    go sec cur [] = flush sec cur []
    indent = B.length . C.takeWhile (== ' ')
    flush sec cur rest = case reverse cur of
      block@(first : _) | startsRule first -> (sec, toCRLF (dedent block)) : rest
      _ -> rest
    -- an unindented line starting with a section number
    heading l
      | " " `B.isPrefixOf` l = Nothing
      | Just rest <- B.stripPrefix "Appendix " l = Just (C.takeWhile (/= '.') rest)
      | otherwise = case C.span (\c -> isDigit c || c == '.') l of
          (n, rest) | not (B.null n), " " `B.isPrefixOf` rest -> Just (C.dropWhileEnd (== '.') n)
          _ -> Nothing

-- | Remove the indentation shared by all the lines that are not
-- blank, and the blank lines at either end.
dedent :: [ByteString] -> [ByteString]
dedent ls =
  let trimmed = reverse (dropWhile isBlank (reverse (dropWhile isBlank ls)))
      indent  = minimum (maxBound : [ B.length (C.takeWhile (== ' ') l)
                                    | l <- trimmed, not (isBlank l) ])
  in map (B.drop indent) trimmed

-- | Join lines with CRLF, ending the last one too.
toCRLF :: [ByteString] -> ByteString
toCRLF = B.concat . map (<> "\r\n")

isBlank :: ByteString -> Bool
isBlank = C.all (== ' ')
