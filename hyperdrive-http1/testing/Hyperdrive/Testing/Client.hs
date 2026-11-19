{-# LANGUAGE OverloadedStrings #-}
-- | A client reading responses, written without the server's code.
module Hyperdrive.Testing.Client
  ( Resp(..)
  , readResponses
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (toLower)
import Numeric (readHex)
------------------------------------------------------------------------------
-- a client reading the responses
------------------------------------------------------------------------------

data Resp = Resp
  { rsStatus   :: Int
  , rsFields   :: [(ByteString, ByteString)]  -- ^ names in lower case
  , rsBody     :: ByteString
  , rsTrailers :: [(ByteString, ByteString)]
  , rsInterim  :: Int                          -- ^ 100 (Continue) responses before it
  } deriving (Eq, Show)

-- | Read responses as RFC 9112 Section 6.3 tells a client to, given
-- whether each answers a HEAD request. Written without the server's
-- code.
readResponses :: [Bool] -> ByteString -> Either String [Resp]
readResponses heads = go heads 0
  where
    go hs interim s
      | B.null s = Right []
      | otherwise = do
          (status, fs, rest) <- readHead s
          if status == 100 then go hs (interim + 1) rest else do
            let isHead = case hs of { (x : _) -> x; [] -> False }
                header n = lookup n fs
            (body, ts, rest') <-
              if isHead || status `elem` [204, 304] then Right ("", [], rest)
              else if fmap (map toLower . C.unpack) (header "transfer-encoding") == Just "chunked" then chunked rest
              else case header "content-length" of
                Just n | [(len, "")] <- reads (C.unpack n) ->
                  if B.length rest < len then Left "short body" else Right (B.take len rest, [], B.drop len rest)
                Just _ -> Left "bad Content-Length"
                Nothing -> Right (rest, [], "")
            (Resp status fs body ts interim :) <$> go (drop 1 hs) 0 rest'
    readHead s = case B.breakSubstring "\r\n\r\n" s of
      (_, r) | B.null r -> Left ("no end of head: " ++ show s)
      (h, r) -> case C.lines (C.filter (/= '\r') h) of
        (line : fls) | Just st <- statusOf line -> Right (st, map field fls, B.drop 4 r)
        _ -> Left ("bad status line: " ++ show h)
    statusOf line = case C.words line of
      ("HTTP/1.1" : code : _) | [(n, "")] <- reads (C.unpack code), " " `B.isPrefixOf` B.drop 12 line -> Just n
      _ -> Nothing
    field l = let (n, v) = C.break (== ':') l in (C.map toLower n, C.dropWhile (== ' ') (B.drop 1 v))
    chunked s = case B.breakSubstring "\r\n" s of
      (line, r) | B.null r -> Left "no chunk size"
                | [(n, "")] <- readHex (C.unpack line) ->
                    if n == 0 then trailers (B.drop 2 r) []
                    else let d = B.drop 2 r in
                      if B.take 2 (B.drop n d) /= "\r\n" then Left "bad chunk end"
                      else (\(b, ts, rest) -> (B.take n d <> b, ts, rest)) <$> chunked (B.drop (n + 2) d)
                | otherwise -> Left ("bad chunk size " ++ show line)
    trailers s acc = case B.breakSubstring "\r\n" s of
      (_, r) | B.null r -> Left "no end of trailers"
      ("", r) -> Right ("", reverse acc, B.drop 2 r)
      (l, r) -> trailers (B.drop 2 r) (field l : acc)

