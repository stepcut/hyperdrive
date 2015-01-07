{-# LANGUAGE TemplateHaskell #-}
module Hyperdrive.Parser.ABNF.Attoparsec where

import ABNF.ClassyParser.Gen.Attoparsec
import Data.Attoparsec.ByteString.Char8
import Hyperdrive.Parser.ABNF.Parser
import Hyperdrive.Types           (RequestParser, RequestRaw)

requestParser :: Parser RequestRaw
requestParser = $(runGenAttoparsec request_raw_parser)
