{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
-- | A reference model of an HTTP/1.1 connection, reading a whole
-- session at once.
module Hyperdrive.Testing.Model
  ( Seen
  , pairs
  , model
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Hyperdrive.HTTP1.Chunked
import Hyperdrive.HTTP1.Connection (Config(..), defaultConfig)
import Hyperdrive.HTTP1.Framing (RequestFraming(..), framingErrorStatus, requestFraming)
import Hyperdrive.HTTP1.Head (HeadResult(..), RawRequestHead, defaultLimits, headErrorStatus, parseRequestHead)
import qualified Hyperdrive.HTTP1.Head as Head
import Hyperdrive.HTTP1.Target (reconstructTarget, targetErrorStatus)
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.FieldSyntax (Comments(..), splitList)
import Hyperdrive.Semantics.Message (HttpVersion(..), isProtocolField)
import Hyperdrive.Semantics.Method (methodBytes)
import Hyperdrive.Semantics.Status (Status, mkStatus, statusCode, pattern BadRequest)
import Hyperdrive.Semantics.Target (Target)
import Hyperdrive.Testing.Client
import Hyperdrive.Testing.Session
------------------------------------------------------------------------------
-- the reference model
------------------------------------------------------------------------------

-- | What the application sees of a request: method, target, version,
-- fields, body length; the body read, its trailers, and whether it was
-- aborted.
type Seen = (ByteString, String, HttpVersion, [(ByteString, ByteString)], Maybe Integer, ByteString, Maybe [(ByteString, ByteString)], Bool)

pairs :: Fields -> [(ByteString, ByteString)]
pairs fs = [ (fieldNameBytes k, fieldValueBytes v) | (k, v) <- fieldsToList fs ]

-- | The requests the application should see and the responses a client
-- should read, for the whole input at once, followed by the end of the
-- input, with scripts that read all of a body or none of it.
--
-- Each request is taken from the start of what is left: its head with
-- 'parseRequestHead', its framing, target and fields with the functions
-- that are tested on their own, and its body all at once (a
-- Content-Length body is the next n octets; a chunked one is decoded
-- in one go). The rest follows RFC 9112 Section 9 and the design (D2,
-- D3, D5 to D7, Q2, Q3), written out case by case.
model :: [Script] -> ByteString -> ([Seen], [Resp])
model = go
  where
    limit = toInteger (configDiscardLimit defaultConfig)
    go scripts input = case parseRequestHead defaultLimits 0 input of
      HeadIncomplete _ -> ([], [])                                   -- the client closed
      HeadError e -> ([], [rejection (headErrorStatus e)])
      HeadComplete raw used -> case check raw of
        Left st -> ([], [rejection st])
        Right (method, target, version, fs, framing, expects100, closeAsked) ->
          let (sc, scripts') = case scripts of { (x : xs) -> (x, xs); [] -> (readAll, []) }
              rest = B.drop used input
              hasBody = framing /= NoBody && framing /= ContentLength 0
              len = case framing of { NoBody -> Just 0; ContentLength n -> Just (toInteger n); Chunked -> Nothing }
              seenAs body end aborted = (method, show target, version, fs, len, body, end, aborted)
              respondThen = respond sc method version closeAsked expects100 scripts'
          in if not hasBody
               then add (seenAs "" Nothing False) (respondThen False (Just rest))
             else if sReads sc == Nothing
               -- reading the body sends 100 (Continue) first, if expected (D7)
               then case wholeBody framing rest of
                 -- the data before the body was found to be cut short or invalid has been read
                 (partial, Nothing) -> ([seenAs partial Nothing True], [(rejection BadRequest) { rsInterim = fromEnum expects100 }])
                 (body, Just (trailers, k)) ->
                   firstInterim expects100 (add (seenAs body (Just trailers) False) (respondThen False (Just (B.drop k rest))))
               -- D6: a body nobody read is dropped after the response, if
               -- all of it arrives within the limit
               else add (seenAs "" Nothing False) (respondThen True (discarded framing rest))
    add v (served, resps) = (v : served, resps)
    firstInterim e (served, r : rs) = (served, r { rsInterim = fromEnum e } : rs)
    firstInterim _ x = x

    -- the response, then the next request if the connection is kept
    respond sc method version closeAsked expects100 scripts' unread next =
      let code = sStatus sc
          noContent = code `elem` [204, 304]
          isHead = method == "HEAD"
          len = sum (map B.length (sContent sc))
          trailers = not noContent && not (null (sTrailers sc))
          -- framing, as RFC 9112 Section 6 and design note M2 have it
          (framingFs, chunked)
            | noContent = ([], False)
            | isHead = (if sDeclared sc then [("content-length", C.pack (show len))] else [], False)
            | version == HttpVersion 1 1 && (not (sDeclared sc) || trailers) = ([("transfer-encoding", "chunked")], True)
            | sDeclared sc = ([("content-length", C.pack (show len))], False)
            | otherwise = ([], False)
          -- D5, RFC 9112 Section 9.3; and a client waiting for 100 may never send its body
          keepAlive = version == HttpVersion 1 1 && not closeAsked && not (unread && expects100)
          fs = [("x-test", "1")] ++ framingFs
               ++ [ ("trailer", B.intercalate ", " [ C.map toLower n | (n, _) <- sTrailers sc ]) | chunked, trailers ]
               ++ [ ("connection", "close") | not keepAlive ]
          resp = Resp code fs (if noContent || isHead then "" else B.concat (sContent sc))
                      (if chunked then [ (C.map toLower n, v) | (n, v) <- sTrailers sc ] else []) 0
          (served, resps) = case next of
            Just rest | keepAlive -> go scripts' rest
            _                     -> ([], [])
      in (served, resp : resps)

    discarded framing rest = case wholeBody framing rest of
      (body, Just (_, k))
        | toInteger (B.length body) <= limit, toInteger k <= limit || framing /= Chunked -> Just (B.drop k rest)
      _ -> Nothing

    -- the checks on a request head, in the order they are made
    check :: RawRequestHead -> Either Status (ByteString, Target, HttpVersion, [(ByteString, ByteString)], RequestFraming, Bool, Bool)
    check raw = do
      let version = Head.headVersion raw
          method = Head.headMethod raw
          hfs = Head.headFields raw
          lowerAll = map (C.map toLower)
          members n = fmap concat (traverse (either (const Nothing) Just . splitList NoComments) (lookupAll (name n) hfs))
      framing <- either (Left . framingErrorStatus) Right (requestFraming version hfs)
      (target, fields') <- either (Left . targetErrorStatus) Right
                             (reconstructTarget (configTarget defaultConfig) version method (Head.headTarget raw) hfs)
      if methodBytes method == "CONNECT" && framing /= NoBody then Left BadRequest else Right ()
      expects <- case members "Expect" of
        Just ms | all (== "100-continue") (lowerAll ms) -> Right (not (null ms))
        _ -> Left (status 417)
      closeAsked <- case members "Connection" of
        Just os | all (\o -> not (B.null o) && C.all (`elem` tchars) o) os -> Right ("close" `elem` lowerAll os)
        _ -> Left BadRequest
      let hasBody = framing /= NoBody && framing /= ContentLength 0
          fields'' = [ p | p@(k, _) <- pairs fields', not (isProtocolField (name k)) ]
      Right ( methodBytes method, target, version, fields'', framing
            , expects && version == HttpVersion 1 1 && hasBody, closeAsked )
    tchars = "!#$%&'*+-.^_`|~0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" :: String

    -- the body's content, and its trailers and length on the wire, or
    -- Nothing if it is cut short or invalid (the content is then what
    -- came before that)
    wholeBody framing rest = case framing of
      ContentLength n | toInteger (B.length rest) >= toInteger n -> (B.take (fromIntegral n) rest, Just ([], fromIntegral n))
                      | otherwise -> (rest, Nothing)
      Chunked -> dechunk startDecoder rest 0 ""
      NoBody -> ("", Just ([], 0))
    dechunk d s used acc = case decodeChunked defaultChunkedLimits d s of
      Data b k d' -> dechunk d' (B.drop k s) (used + k) (acc <> b)
      NeedMore k d' | k > 0 -> dechunk d' (B.drop k s) (used + k) acc
      Finished t k -> (acc, Just (pairs t, used + k))
      _ -> (acc, Nothing)

    rejection st = Resp (statusCode st) [("content-length", "0"), ("connection", "close")] "" [] 0
    status = fromJust . mkStatus

