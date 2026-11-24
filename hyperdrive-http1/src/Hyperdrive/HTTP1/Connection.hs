{-# LANGUAGE BangPatterns, OverloadedStrings, PatternSynonyms #-}
-- | The HTTP/1.1 connection, without IO: 'step' folds what happens on a
-- connection ('Input') into its state ('Conn') and says what to do
-- about it ('Output'). An IO driver reads and writes the socket, runs
-- the application and the timers, and feeds the results back in.
--
-- See docs/http1-core-design.md for the design, and the decisions
-- (D1 to D11, Q1 to Q5) referred to here.
module Hyperdrive.HTTP1.Connection
  ( -- * Configuration
    Config(..)
  , defaultConfig
    -- * Requests and responses as the driver sees them
  , RequestHead(..)
  , ResponseHead(..)
    -- * Stepping
  , Input(..)
  , Output(..)
  , Misuse(..)
  , Conn
  , newConn
  , step
  , Phase(..)
  , phase
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Array (Array, listArray, (!))
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Maybe (fromJust, fromMaybe, isJust)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (pokeByteOff)
import Data.Word (Word64, Word8)
import Numeric (showHex)
import Hyperdrive.HTTP1.Chunked
import Hyperdrive.HTTP1.Framing
import Hyperdrive.HTTP1.Head
import Hyperdrive.HTTP1.Target
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.FieldSyntax (Comments(..), splitList)
import Hyperdrive.Semantics.Internal (FieldName(..), FieldValue(..), Fields(..))
import Hyperdrive.Semantics.Message (HttpVersion, isProtocolField, pattern HTTP_1_1)
import Hyperdrive.Semantics.Method (Method, pattern CONNECT)
import Hyperdrive.Semantics.Status
import Hyperdrive.Semantics.Syntax (isTChar)
import Hyperdrive.Semantics.Target (Target)
import Hyperdrive.Semantics.URI (pattern HTTP)

------------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------------

data Config = Config
  { configHeadLimits    :: !Limits         -- ^ D4
  , configChunkLimits   :: !ChunkedLimits  -- ^ D4
  , configTarget        :: !TargetContext  -- ^ the connection's scheme, and a default authority
  , configDiscardLimit  :: !Word64
    -- ^ how much of a request body the application did not read is
    -- read and discarded after its response, to keep the connection
    -- (D6)
  } deriving (Eq, Show)

-- | The D4 limits, 64 KiB to discard, and an unsecured connection
-- with no default authority.
defaultConfig :: Config
defaultConfig = Config
  { configHeadLimits   = defaultLimits
  , configChunkLimits  = defaultChunkedLimits
  , configTarget       = TargetContext HTTP Nothing
  , configDiscardLimit = 65536
  }

------------------------------------------------------------------------------
-- Requests and responses
------------------------------------------------------------------------------

-- | A request, ready for 'Hyperdrive.Semantics.Message.mkRequest' and
-- 'Hyperdrive.Semantics.Message.setAcceptsTrailers'.
data RequestHead = RequestHead
  { reqVersion         :: !HttpVersion
  , reqMethod          :: !Method
  , reqTarget          :: !Target
  , reqFields          :: !Fields
    -- ^ without @Host@ (it is in the target) or protocol fields
  , reqBodyLength      :: !(Maybe Word64)
    -- ^ @Just 0@: no body, and no 'RequestData' or 'RequestEnd' will
    -- follow. @Nothing@: chunked, length unknown.
  , reqAcceptsTrailers :: !Bool
  } deriving (Show)

-- | A final response, as the driver makes it from a
-- 'Hyperdrive.Semantics.Message.Response'.
data ResponseHead = ResponseHead
  { respStatus     :: !Status
  , respFields     :: !Fields         -- ^ with @Date@ added by the driver (M4)
  , respHasContent :: !Bool           -- ^ 'False' for @ContentNone@
  , respLength     :: !(Maybe Word64) -- ^ the content's length, if known
  , respTrailers   :: ![FieldName]    -- ^ the trailer fields declared (M6)
  } deriving (Show)

------------------------------------------------------------------------------
-- Inputs and outputs
------------------------------------------------------------------------------

data Input
  = Received !ByteString   -- ^ bytes from the socket
  | PeerClosed             -- ^ the client closed its side (end of input)
  | Respond !ResponseHead  -- ^ the application's response to the current request
  | WriteBody !ByteString  -- ^ some of the response's content
  | EndBody !Fields        -- ^ the end of the response's content, with its trailers
  | Abort                  -- ^ the application failed while sending its response
  | WantBody               -- ^ the application wants the next chunk of the request body (Q1)
  | TimedOut               -- ^ the timer for the current 'phase' expired (D8)
  | Shutdown               -- ^ finish the current exchange, if any, then close
  deriving (Show)

data Output
  = NewRequest !RequestHead   -- ^ a request arrived; answer it before the next one is read (D2)
  | RequestData !ByteString  -- ^ answers a 'WantBody': part of the body, a slice of what was received
  | RequestEnd !Fields       -- ^ the end of the body, with its trailers
  | RequestAborted           -- ^ the body will not arrive in full: @readChunk@ throws @IncompleteBody@
  | Send ![ByteString]       -- ^ write these, in order
  | NeedInput                -- ^ read from the socket, and pass on what arrives
  | Close
    -- ^ close the connection, after sending what was asked: half-close,
    -- then read until the client closes or a short timer expires (RFC
    -- 9112 Section 9.6). Every later input is ignored.
  | ProtocolMisuse !Misuse   -- ^ the driver did something out of order; 'Close' follows (Q4)
  deriving (Show)

-- | An input that made no sense in the connection's state: a bug in the
-- driver, never the client's or the application's doing.
data Misuse
  = RespondWithoutRequest
  | RespondTwice
  | BodyWithoutResponse      -- ^ 'WriteBody' or 'EndBody' with no response started
  | WantBodyWithoutBody      -- ^ 'WantBody' with no body left to read
  | WantBodyTwice            -- ^ 'WantBody' while one is unanswered
  | TimedOutWithoutTimer     -- ^ 'TimedOut' in a phase that has no timer
  deriving (Eq, Show)

-- | Which timer the driver runs (D8).
data Phase
  = Idle                   -- ^ waiting for a request: close quietly on timeout
  | ReadingHead            -- ^ part of a request head has arrived: 408 on timeout
  | ReadingBody            -- ^ waiting for body bytes the application asked for
  | DiscardingBody         -- ^ reading the rest of a body nobody wants (D6)
  | WaitingForApplication  -- ^ no protocol timer: the application is working
  | Closed
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- State
------------------------------------------------------------------------------

data Conn = Conn
  { cConfig     :: !Config
  , cBuffer     :: !ByteString      -- ^ received and not yet used
  , cRequest    :: !RequestSide
  , cExchange   :: !(Maybe Exchange) -- ^ the request being answered, from its head to its response's end
  , cShutdown   :: !Bool
  , cPeerClosed :: !Bool
  , cClosed     :: !Bool
  }

data RequestSide
  = AwaitingHead !Int        -- ^ how far the buffer has been searched for the end of a head
  | InBody !Body !Bool       -- ^ the body, and whether a 'WantBody' is waiting for an answer
  | Discarding !Body !Word64 -- ^ the body, and how much more may be discarded
  | RequestDone

data Body = Remaining !Word64 | ChunkedBody !Decoder

data Exchange = Exchange
  { exVersion      :: !HttpVersion
  , exMethod       :: !Method
  , exKeepAlive    :: !Bool  -- ^ the request allows the connection to be reused
  , exExpects100   :: !Bool  -- ^ @Expect: 100-continue@, not yet answered
  , exResponse     :: !ResponseSide
  }

data ResponseSide
  = AwaitingResponse
  | Sending !ResponseFraming !Word64  -- ^ the framing, and the octets of content written
  | ResponseDone

newConn :: Config -> Conn
newConn config = Conn config B.empty (AwaitingHead 0) Nothing False False False

phase :: Conn -> Phase
phase c
  | cClosed c = Closed
  | otherwise = case cRequest c of
      AwaitingHead _ | B.null (cBuffer c) -> Idle
                     | otherwise          -> ReadingHead
      InBody _ True   -> ReadingBody
      Discarding _ _  -> DiscardingBody
      _               -> WaitingForApplication

------------------------------------------------------------------------------
-- Stepping
------------------------------------------------------------------------------

step :: Input -> Conn -> (Conn, [Output])
step _ c | cClosed c = result c []
step input c = case input of
  Received bs -> advance c { cBuffer = if B.null (cBuffer c) then bs else cBuffer c <> bs }
  PeerClosed -> case cRequest c of
    AwaitingHead _ -> close c []
    _              -> advance c { cPeerClosed = True }
  Shutdown -> case phase c of
    Idle -> close c []
    _    -> result c { cShutdown = True } []
  TimedOut -> case phase c of
    Idle           -> close c []
    ReadingHead    -> reject RequestTimeout c
    ReadingBody    -> aborted RequestTimeout c
    DiscardingBody -> close c []
    _              -> misuse TimedOutWithoutTimer c
  WantBody -> case (cRequest c, cExchange c) of
    (InBody _ True, _) -> misuse WantBodyTwice c
    (InBody body False, Just ex) ->
      -- = https://www.rfc-editor.org/rfc/rfc9110#section-10.1.1
      -- # Upon receiving an HTTP/1.1 (or later) request that has a method,
      -- # target URI, and complete header section that contains a 100-continue
      -- # expectation and an indication that request content will follow, an
      -- # origin server MUST send either:

      -- = https://www.rfc-editor.org/rfc/rfc9110#section-10.1.1
      -- # The origin server MUST NOT wait for the content before sending the
      -- # 100 (Continue) response.

      -- The application either responds (a final status) or asks for the
      -- body, which sends 100 (Continue) at once, before any of it is
      -- waited for.
      let continue = exExpects100 ex && isAwaiting (exResponse ex)
          c' = c { cRequest = InBody body True, cExchange = Just $! ex { exExpects100 = False } }
      in prefix [Send ["HTTP/1.1 100 Continue\r\n\r\n"] | continue] (advance c')
    _ -> misuse WantBodyWithoutBody c
  Respond h -> case cExchange c of
    Nothing -> misuse RespondWithoutRequest c
    Just ex | not (isAwaiting (exResponse ex)) -> misuse RespondTwice c
            | otherwise -> respond h ex c
  WriteBody bs -> case cExchange c of
    Just ex | Sending framing written <- exResponse ex -> writeBody bs framing written ex c
    _ -> misuse BodyWithoutResponse c
  EndBody trailers -> case cExchange c of
    Just ex | Sending framing written <- exResponse ex -> endBody trailers framing written ex c
    _ -> misuse BodyWithoutResponse c
  Abort -> close c []

-- | Do whatever the buffered input allows, in the current state.
advance :: Conn -> (Conn, [Output])
advance c = case cRequest c of
  AwaitingHead searched -> readHead searched c
  InBody body True      -> readBody body c
  InBody _ False        -> result c []
  Discarding body left  -> discard body left c
  RequestDone           -> result c []

------------------------------------------------------------------------------
-- Request heads
------------------------------------------------------------------------------

readHead :: Int -> Conn -> (Conn, [Output])
readHead searched c =
  case parseRequestHead (configHeadLimits (cConfig c)) searched (cBuffer c) of
    HeadIncomplete n
      | cPeerClosed c -> close c []
      | otherwise     -> result c { cRequest = AwaitingHead n } [NeedInput]
    HeadError e -> reject (headErrorStatus e) c
    HeadComplete raw used -> case startRequest (cConfig c) raw of
      Left status -> reject status c
      Right (h, body, expects100, keepAlive) ->
        let !ex = Exchange (reqVersion h) (reqMethod h) keepAlive expects100 AwaitingResponse
        in result c { cBuffer = B.drop used (cBuffer c)
                    , cRequest = maybe RequestDone (\b -> InBody b False) body
                    , cExchange = Just ex }
                  [NewRequest h]

-- | Everything about a request head that can make it unacceptable,
-- giving the head, its body, whether it expects 100 (Continue), and
-- whether it allows the connection to be reused.
startRequest :: Config -> RawRequestHead -> Either Status (RequestHead, Maybe Body, Bool, Bool)
startRequest config raw = do
  let version = headVersion raw
      method = headMethod raw
      (fields, found) = splitFields (headLinesReversed raw)
  framing <- either (Left . framingErrorStatus) Right
               (requestFramingFrom version (foundTransferEncoding found) (foundContentLength found))
  target <- either (Left . targetErrorStatus) Right
              (reconstructTargetFrom (configTarget config) version method (headTarget raw) (foundHost found))
  -- Q2: CONNECT has no content (RFC 9110 Section 9.3.6, M10)
  if method == CONNECT && framing /= NoBody then Left BadRequest else Right ()
  expects100 <- expectation version (foundExpect found)
  closeRequested <- maybe (Left BadRequest) Right (connectionClose (foundConnection found))
  let (body, len) = case framing of
        NoBody          -> (Nothing, Just 0)
        ContentLength 0 -> (Nothing, Just 0)
        ContentLength n -> (Just (Remaining n), Just n)
        Chunked         -> (Just (ChunkedBody startDecoder), Nothing)
      h = RequestHead version method target fields len (acceptsTrailersFrom (foundTE found))
  -- = https://www.rfc-editor.org/rfc/rfc9112#section-9.3
  -- # If the received protocol is HTTP/1.1 (or later), the connection
  -- # will persist after the current response
  Right (h, body, expects100 && isJust body, version >= HTTP_1_1 && not closeRequested)

-- | A request's fields, split in one pass: the fields the application
-- gets, which are those that are neither Host nor a protocol field,
-- and the values of the fields the core interprets. Looked up one name
-- at a time, the fields were walked seven times.
data Split = Split ![(FieldName, FieldValue)] !CoreFields

-- | The values of the fields the core interprets, each in order. Apart
-- from the application's fields, so that adding one of those (most
-- fields) does not copy these.
data CoreFields = CoreFields
  { foundHost             :: ![FieldValue]
  , foundTransferEncoding :: ![FieldValue]
  , foundContentLength    :: ![FieldValue]
  , foundTE               :: ![FieldValue]
  , foundConnection       :: ![FieldValue]
  , foundExpect           :: ![FieldValue]
  }

--
-- The lines come the last one first, as the head was read, so putting
-- each at the front of its list leaves every list in the order
-- received, and the application's fields are built once.
splitFields :: [(FieldName, FieldValue)] -> (Fields, CoreFields)
splitFields reversed = inOrder (F.foldl' place (Split [] (CoreFields [] [] [] [] [] [])) reversed)
  where
    -- by length first: most names are then ruled out by one comparison
    place (Split rest found) f@(name@(FieldName n), v) = case B.length n of
      2  | n == "te"                -> Split rest found { foundTE = v : foundTE found }
      4  | n == "host"              -> Split rest found { foundHost = v : foundHost found }
      -- Expect is not a protocol field: the application sees it too
      6  | n == "expect"            -> Split (f : rest) found { foundExpect = v : foundExpect found }
      10 | n == "connection"        -> Split rest found { foundConnection = v : foundConnection found }
      14 | n == "content-length"    -> Split rest found { foundContentLength = v : foundContentLength found }
      17 | n == "transfer-encoding" -> Split rest found { foundTransferEncoding = v : foundTransferEncoding found }
      _  | isProtocolField name     -> Split rest found
         | otherwise                -> Split (f : rest) found
    inOrder (Split rest found) = (Fields (Seq.fromList rest), found)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-10.1.1
-- # A server that receives an Expect field value containing a member
-- # other than 100-continue MAY respond with a 417 (Expectation Failed)
-- # status code to indicate that the unexpected expectation cannot be
-- # met.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-10.1.1
-- # *  A server that receives a 100-continue expectation in an HTTP/1.0
-- # request MUST ignore that expectation.

-- | Whether the request expects 100 (Continue). Any other expectation,
-- or an @Expect@ that is not a list, is 417 (Q3).
expectation :: HttpVersion -> [FieldValue] -> Either Status Bool
expectation version values =
  case traverse (either (const Nothing) Just . splitList NoComments) values of
    Nothing -> Left ExpectationFailed
    Just members
      | all ((== "100-continue") . lower) (concat members) -> Right (version >= HTTP_1_1 && not (null (concat members)))
      | otherwise -> Left ExpectationFailed

-- = https://www.rfc-editor.org/rfc/rfc9112#section-9.6
-- # A server that receives a "close" connection option MUST initiate
-- # closure of the connection (see below) after it sends the final
-- # response to the request that contained the "close" connection
-- # option.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-7.6.1
-- # Connection        = #connection-option
-- # connection-option = token

-- | Whether @Connection@ has the @close@ option, or 'Nothing' if it is
-- not a list of tokens.
connectionClose :: [FieldValue] -> Maybe Bool
connectionClose [] = Just False
-- the usual field is one line with one option ("keep-alive" from a
-- browser): a token is the whole list, and nothing is built to read it
connectionClose [value]
  | B.all isTChar option = Just (B.length option == 5 && lower option == "close")
  where option = fieldValueBytes value
connectionClose values = do
  options <- concat <$> traverse (either (const Nothing) Just . splitList NoComments) values
  if all (\o -> not (B.null o) && B.all isTChar o) options then Just ("close" `elem` map lower options) else Nothing

lower :: ByteString -> ByteString
lower = C.map (\ch -> if ch >= 'A' && ch <= 'Z' then toEnum (fromEnum ch + 32) else ch)

------------------------------------------------------------------------------
-- Request bodies
------------------------------------------------------------------------------

-- | Answer the waiting 'WantBody' from the buffer, or ask for input.
readBody :: Body -> Conn -> (Conn, [Output])
readBody body c = case body of
  Remaining n
    | B.null buf -> needInput
    | otherwise ->
        let piece = B.take (fromIntegral (min n (fromIntegral (B.length buf)))) buf
            left = n - fromIntegral (B.length piece)
            c' = c { cBuffer = B.drop (B.length piece) buf }
        in if left == 0
             then requestDone c' [RequestData piece, RequestEnd emptyFields]
             else result c' { cRequest = InBody (Remaining left) False } [RequestData piece]
  ChunkedBody d -> case decodeChunked (configChunkLimits (cConfig c)) d buf of
    Data piece used d' -> result c { cBuffer = B.drop used buf, cRequest = InBody (ChunkedBody d') False } [RequestData piece]
    NeedMore used d' -> readBodyMore c { cBuffer = B.drop used buf, cRequest = InBody (ChunkedBody d') True }
    Finished trailers used -> requestDone c { cBuffer = B.drop used buf } [RequestEnd trailers]
    DecodeError _ -> aborted BadRequest c
  where
    buf = cBuffer c
    needInput = readBodyMore c
    readBodyMore c'
      | cPeerClosed c' = aborted BadRequest c'
      | otherwise      = result c' [NeedInput]

-- | The request body has been read in full.
requestDone :: Conn -> [Output] -> (Conn, [Output])
requestDone c outs = case cExchange c of
  Just ex | ResponseDone <- exResponse ex -> prefix outs (finishExchange c { cRequest = RequestDone })
  _ -> result c { cRequest = RequestDone } outs

-- | Read and drop the rest of a body after its response (D6), up to the
-- limit.
discard :: Body -> Word64 -> Conn -> (Conn, [Output])
discard body left c = case body of
  Remaining n
    | n > left -> close c []
    | B.null buf -> more c
    | otherwise ->
        let k = min n (fromIntegral (B.length buf))
            c' = c { cBuffer = B.drop (fromIntegral k) buf }
        in if k == n then finishExchange c' { cRequest = RequestDone }
           else more c' { cRequest = Discarding (Remaining (n - k)) (left - k) }
  ChunkedBody d -> case decodeChunked (configChunkLimits (cConfig c)) d buf of
    Data _ used d' -> spend used (ChunkedBody d')
    NeedMore used d' -> case charge used of
      Nothing    -> close c []
      Just left' -> more c { cBuffer = B.drop used buf, cRequest = Discarding (ChunkedBody d') left' }
    Finished _ used -> case charge used of
      Nothing -> close c []
      Just _  -> finishExchange c { cBuffer = B.drop used buf, cRequest = RequestDone }
    DecodeError _ -> close c []
  where
    buf = cBuffer c
    charge used = if fromIntegral used > left then Nothing else Just (left - fromIntegral used)
    spend used body' = case charge used of
      Nothing    -> close c []
      Just left' -> discard body' left' c { cBuffer = B.drop used buf }
    more c'
      | cPeerClosed c' = close c' []
      | otherwise      = result c' [NeedInput]

------------------------------------------------------------------------------
-- Responses
------------------------------------------------------------------------------

respond :: ResponseHead -> Exchange -> Conn -> (Conn, [Output])
respond h ex c =
  let payload = if respHasContent h then Just (Payload (respLength h) (not (null (respTrailers h)))) else Nothing
      framing = responseFraming (exVersion ex) (exMethod ex) (respStatus h) payload
      bodyUnread = case cRequest c of
        InBody _ _ -> True
        _          -> False
      -- = https://www.rfc-editor.org/rfc/rfc9110#section-10.1.1
      -- # *  A server that responds with a final status code before reading the
      -- # entire request content SHOULD indicate whether it intends to close
      -- # the connection (e.g., see Section 9.6 of [HTTP/1.1]) or continue
      -- # reading the request content.
      -- A client still waiting for 100 (Continue) may never send the
      -- body, so its connection cannot be reused.
      -- (A close-delimited response is only sent to HTTP/1.0, which
      -- never keeps the connection.)
      keepAlive = exKeepAlive ex && not (cShutdown c) && not (bodyUnread && exExpects100 ex)
      trailerField = [ (hTrailer, v)
                     | framing == SendChunked, not (null (respTrailers h))
                     , Right v <- [mkFieldValue (B.intercalate ", " (map fieldNameBytes (respTrailers h)))] ]
      closeField = [ (hConnection, closeValue) | not keepAlive ]
      headBytes = renderHead (statusLine (respStatus h)) (respFields h)
                    (framingFields framing ++ trailerField ++ closeField)
      !ex' = ex { exKeepAlive = keepAlive, exResponse = Sending framing 0 }
  in result c { cExchange = Just ex' } [Send [headBytes]]

writeBody :: ByteString -> ResponseFraming -> Word64 -> Exchange -> Conn -> (Conn, [Output])
writeBody bs framing written ex c =
  let written' = written + fromIntegral (B.length bs)
      c' = c { cExchange = Just $! ex { exResponse = Sending framing written' } }
  in case framing of
       _ | B.null bs  -> result c []
       HeaderOnly _   -> result c' []
       SendLength n
         | written' > n -> close c []   -- more than declared: never sent (Abort)
         | otherwise    -> result c' [Send [bs]]
       SendChunked    -> result c' [Send [C.pack (showHex (B.length bs) "") <> "\r\n", bs, "\r\n"]]
       SendUntilClose -> result c' [Send [bs]]

endBody :: Fields -> ResponseFraming -> Word64 -> Exchange -> Conn -> (Conn, [Output])
endBody trailers framing written ex c = case framing of
  SendLength n | written < n -> close c []   -- less than declared: aborted
  SendChunked    -> prefix [Send [renderHead "0\r\n" trailers []]] done
  SendUntilClose -> close c []
  _              -> done
  where
    done = responseDone c { cExchange = Just $! ex { exResponse = ResponseDone } }

-- | The response has been sent in full.
responseDone :: Conn -> (Conn, [Output])
responseDone c = case (cRequest c, cExchange c) of
  (RequestDone, _) -> finishExchange c
  (InBody body _, Just ex)
    | exKeepAlive ex -> discard body (configDiscardLimit (cConfig c)) c { cRequest = Discarding body (configDiscardLimit (cConfig c)) }
  _ -> close c []

-- | Both the request and its response are complete: close, or go on to
-- the next request, which may already be in the buffer.
finishExchange :: Conn -> (Conn, [Output])
finishExchange c = case cExchange c of
  Just ex | exKeepAlive ex, not (cShutdown c) -> advance c { cRequest = AwaitingHead 0, cExchange = Nothing }
  _ -> close c []

-- | The status line, made once for each status code.
statusLine :: Status -> ByteString
statusLine s = statusLines ! statusCode s

-- | Every 'Status' has a code from 100 to 599.
statusLines :: Array Int ByteString
statusLines = listArray (100, 599) (map makeStatusLine [100 .. 599])

makeStatusLine :: Int -> ByteString
makeStatusLine code = "HTTP/1.1 " <> C.pack (show code) <> " " <> fromMaybe "" (lookup code reasonPhrases) <> "\r\n"

-- | A status line and field lines, and the empty line that ends them,
-- written into one buffer of the exact size, rather than joined from
-- several small strings per field. The field lines are a section's
-- and then some more (the core's own), each folded over where it is
-- rather than joined into one list first.
renderHead :: ByteString -> Fields -> [(FieldName, FieldValue)] -> ByteString
renderHead line (Fields fs) more = BI.unsafeCreate size $ \p -> do
  start <- put p 0 line
  mid <- F.foldlM (fieldLine p) start fs
  end <- F.foldlM (fieldLine p) mid more
  crlf p end
  where
    size = F.foldl' lineLength (F.foldl' lineLength (B.length line + 2) fs) more
    lineLength !acc (FieldName n, FieldValue v) = acc + B.length n + B.length v + 4
    fieldLine p !off (FieldName n, FieldValue v) = do
      off' <- put p off n
      pokeByteOff p off' (0x3A :: Word8)        -- ':'
      pokeByteOff p (off' + 1) (0x20 :: Word8)  -- SP
      off'' <- put p (off' + 2) v
      crlf p off''
      pure (off'' + 2)
    put p off bs = BU.unsafeUseAsCStringLen bs $ \(src, len) ->
      copyBytes (p `plusPtr` off) (castPtr src) len >> pure (off + len)
    crlf p off = pokeByteOff p off (0x0D :: Word8) >> pokeByteOff p (off + 1) (0x0A :: Word8)

hTrailer :: FieldName
hTrailer = fromJust (mkFieldName "trailer")

closeValue :: FieldValue
closeValue = either (error . show) id (mkFieldValue "close")

-- | Reason phrases for the common codes; others are sent without one
-- (the reason-phrase is optional, RFC 9112 Section 4).
reasonPhrases :: [(Int, ByteString)]
reasonPhrases =
  [ (200, "OK"), (201, "Created"), (202, "Accepted"), (203, "Non-Authoritative Information")
  , (204, "No Content"), (205, "Reset Content"), (206, "Partial Content")
  , (300, "Multiple Choices"), (301, "Moved Permanently"), (302, "Found"), (303, "See Other")
  , (304, "Not Modified"), (307, "Temporary Redirect"), (308, "Permanent Redirect")
  , (400, "Bad Request"), (401, "Unauthorized"), (403, "Forbidden"), (404, "Not Found")
  , (405, "Method Not Allowed"), (406, "Not Acceptable"), (408, "Request Timeout")
  , (409, "Conflict"), (410, "Gone"), (411, "Length Required"), (412, "Precondition Failed")
  , (413, "Content Too Large"), (414, "URI Too Long"), (415, "Unsupported Media Type")
  , (416, "Range Not Satisfiable"), (417, "Expectation Failed"), (421, "Misdirected Request")
  , (422, "Unprocessable Content"), (426, "Upgrade Required"), (428, "Precondition Required")
  , (429, "Too Many Requests"), (431, "Request Header Fields Too Large")
  , (500, "Internal Server Error"), (501, "Not Implemented"), (502, "Bad Gateway")
  , (503, "Service Unavailable"), (504, "Gateway Timeout"), (505, "HTTP Version Not Supported") ]

------------------------------------------------------------------------------
-- Errors and closing
------------------------------------------------------------------------------

-- | A request the core answers itself, with a minimal response and a
-- close (D3, Q5).
reject :: Status -> Conn -> (Conn, [Output])
reject status c =
  close c [Send [statusLine status <> "content-length: 0\r\nconnection: close\r\n\r\n"]]

-- | The body will not arrive in full. If no response has started, the
-- core answers with the status; otherwise the response is cut off.
aborted :: Status -> Conn -> (Conn, [Output])
aborted status c = prefix [RequestAborted] $ case cExchange c of
  Just ex | isAwaiting (exResponse ex) -> reject status c
  _ -> close c []

misuse :: Misuse -> Conn -> (Conn, [Output])
misuse m c = close c [ProtocolMisuse m]

close :: Conn -> [Output] -> (Conn, [Output])
close c outs = result c { cClosed = True } (outs ++ [Close])

prefix :: [Output] -> (Conn, [Output]) -> (Conn, [Output])
prefix outs (c, outs') = result c (outs ++ outs')

-- | The new state, evaluated, and the outputs. Built lazily, each state
-- was a thunk that the driver ran at once (ticky counted about twenty
-- a request).
result :: Conn -> [Output] -> (Conn, [Output])
result !c outs = (c, outs)
{-# INLINE result #-}

isAwaiting :: ResponseSide -> Bool
isAwaiting AwaitingResponse = True
isAwaiting _                = False
