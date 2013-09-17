{-# LANGUAGE RankNTypes, DeriveDataTypeable, OverloadedStrings #-}
module Request where

import Control.Applicative
import Control.Exception
import Control.Monad                (forever)
import Pipes
import Pipes.Attoparsec
import Pipes.Parse
import qualified Pipes.Prelude as P
import Control.Exception (Exception, throw)
import Control.Monad.Trans
import Control.Monad.Trans.Error    (ErrorT(runErrorT))
import           Data.Attoparsec.ByteString.Char8 (Parser, string)
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as B
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lex.Integral (readDecimal)
import Data.ByteString.Internal     (c2w)
import Data.ByteString.Unsafe       (unsafeDrop, unsafeIndex, unsafeTake)
import Data.Monoid                  (mappend)
import Data.Typeable                (Typeable)
import Data.Word                    (Word8)
import Network.Socket               (SockAddr(..))
import Types                        (Method(..), Request(..), Response(..), HTTPPipe(..), HTTPVersion(..))
import Pipes.Parse

{-

Concerns:

Header Limits
-------------

We need to limit the size of headers we accept. If we know that we are
going to reject headers over a certain size, and that total header
size is rather small, should we start by drawing all the header data
before we start parsing.

The downside to doing that is we might block on waiting for data when
we could be busy parsing stuff. In general we do not reject headers
for being too big, so we don't care about wasting CPU decoding headers
that could be rejected.

With pipes we are, in theory, interleaving the fetching and
parsing. We fetch data when it is available, parse as much as we can,
and then if we need more, we fetch more. If the parsing is faster than
the transfers, then we willspend time blocked on the fetching, but
when that is happening, we will not have any work around we could be
doing.

Also, while we are parsing,that gives time for more data to arrive. If
transfer is faster than parsing, then the parser will be kept at 100%,
since there will always be data available when it tries to fetch. If
the parsing is slow compared to the fetching, then we might want to
start the parsing as soon as possible. However, if it is much larger,
then waiting a little bit for the data to arrive will not matter. So,
by getting it all at once, we can perhaps eliminate some extra runtime
overhead which will make up for the initial latency in starting to
parse.

If we do not know the relative speeds then jumping back and forth will
give us good performance at the cost of some pontential overhead.

Character Oriented Parsing Efficiency
-------------------------------------

parsing HTTP involves a lot of character orientedp parsing. But we get
ByteStrings in chunks, and don't really want to draw/undraw a single
character at a time from a bytestring.

Additionally, we want to makes sure that when working with a
ByteString, we do not pin the whole thing in RAM when we do not mean
to. That may mean calling an explicit copy on the ByteString values we
want to use directly in our data structure.

When parsing something like the method line, we have two different
types of behaviors. We match on a string that defines a method like
GET or PUT, but we return a constructor value. So, the original
ByteString value is not retained. But for the URI, we want to copy
that part of the bytestring into our Request type. So, there we could
be pinning things.

And, for most headers, we will be copying the key/value pairs into the
Request type. Doing that can cause sneaky problems though.If we are
not careful, we could pin the rest of the ByteString in RAM. Except,
wait! We are not processing a lazy ByteString but rather a stream of
strict ByteString chunks. So each time we draw a new ByteString, we
are getting a new, unconnected ByteString value. So, perhaps that is
not an issue.

Partial Chunk
-------------

When parsing something like the request method, we need to match on a
string like GET, PUT, POST, etc. But, we may not have a complete thing
to match on.



drawWhile :: (Monad m, Proxy p) =>
             (Char -> Bool)
          -> Consumer (StateP [ByteString] p) (Maybe ByteString) m (Maybe ByteString)
drawWhile p =
    do bs <- draw
       case C.span p bs of
         (before, after)
             | C.null after ->
                 do rest <- drawWhile p
       return Nothing
-}

pMethod :: Parser Method
pMethod =
         string "GET"     *> pure GET
     <|> string "POST"    *> pure POST
     <|> string "HEAD"    *> pure HEAD
     <|> string "OPTIONS" *> pure OPTIONS
     <|> string "PUT"     *> pure PUT
     <|> string "DELETE"  *> pure DELETE
     <|> string "CONNECT" *> pure CONNECT
--     <|> ((EXTENSION . C.pack) <$> token)

-- pMethod_testD = runEffect $ evalStateK [] $ runEitherK $ (\() -> respond $ Just "GET") >-> parse pMethod

{-
pMethod_test :: IO (Either ParsingError (), [ByteString])
pMethod_test = runStateT (runErrorT $ runEffect $ ((wrap (\_ -> yield "GET")) >-> parse pMethod >-> (\() -> do c <- await ()  ; liftIO (print c))) ()) []
-}
pRequestURI :: Parser ByteString
pRequestURI = A.takeWhile (/= ' ')

pHTTPVersion :: Parser HTTPVersion
pHTTPVersion =
    string "HTTP/1.1" >> return HTTP11


-- | an almost completely wrong implementation of pToken
pToken :: Parser ByteString
pToken = A.takeWhile (\c -> notElem c " :\r\n")

crlf = A.string "\r\n"

notCR = A.takeWhile (/= '\r')

{-
       message-header = field-name ":" [ field-value ]
       field-name     = token
       field-value    = *( field-content | LWS )
       field-content  = <the OCTETs making up the field-value
                        and consisting of either *TEXT or combinations
                        of token, separators, and quoted-string>
-}


-- | this is also wrong
pMessageHeader :: Parser (ByteString, ByteString)
pMessageHeader =
    do fieldName <- pToken
       A.char8 ':'
       fieldValue <- notCR
       crlf
       return (fieldName, fieldValue)

pRequest :: Bool -> SockAddr -> Parser Request
pRequest secure addr =
    do m <- pMethod
       A.char8 ' '
       uri <- pRequestURI
       A.char8 ' '
       httpVersion <- pHTTPVersion
       crlf
       hdrs <- many pMessageHeader
       crlf
       let request = Request { rqMethod = m
                             , rqURIbs  = uri
                             , rqHTTPVersion = httpVersion
                             , rqHeaders = hdrs
                             , rqSecure = secure
                             , rqClient = addr
                             }
       return request

{-
pRequest_test :: IO (Either ParsingError (), [ByteString])
pRequest_test = runStateT (runEitherT $ runEffect $ ((wrap (\_ -> respond "GET /foo HTTP/1.1")) >-> parse pRequest >-> (\() -> do c <- request ()  ; liftIO (print c))) ()) []
-}
-- parseRequest :: forall m y y'. (Monad m) => Bool -> SockAddr -> Proxy () (Maybe ByteString) y' y (ErrorT ParsingError (StateT [ByteString] m)) Request
parseRequest :: Monad m =>
                Bool
             -> SockAddr
             -> StateT (Producer ByteString m r) m (Either ParsingError (Int, Request))
parseRequest secure clientAddr = parse (pRequest secure clientAddr)

------------------------------------------------------------------------------
-- will remove this garbage shortly, not quite ready to yet
------------------------------------------------------------------------------

-- pMethod_test :: Proxy p => () -> p a b c Method f (Either e r)
--pMethod_test :: (Monad m, Proxy p) =>
--                ()
--             -> p () (Maybe ByteString) () Method m (Either ParsingError (), [ByteString])
-- pMethod_testD = runEffect $ evalStateK [] $ runEitherK $ (\() -> respond $ Just "GET") >-> parseD pMethod
{-
-- pMethod_test2 = runProxy $ evalStateK [] $ runEitherK $ (\() -> respond $ Just "GET") >-> parse pMethod

-- pMethod_test2 = evalStateP [] $ runEitherP $ parse pMethod
-- pMethod_test2 = (wrap (\_ -> respond "GET")) >-> (\_ -> parse pMethod)
--pMethod_test =
--    runProxy $ evalStateK [] $ runEitherK $
--     (\() -> wrap (respond "G" >> respond "ET")) >-> (\() -> parse pMethod)

test_p p bs=
    runProxy $ evalStateK [] $ runEitherK $
     (\() -> wrap (respond bs)) >-> (\() -> parse p)

pMethod_test_1 = test_p pMethod "GET"
pMethod_test_2 = test_p pMethod "GOT"




{-
method :: (Monad m, Proxy p) => StateP [ByteString] p () (Maybe ByteString) y' y m (Maybe Method)
method =
    do mbs <- draw
       case mbs of
         Nothing -> return Nothing
         (Just bs)
             | bs == "GET" -> return (Just GET)

-- requestLine :: (Monad m, Proxy p) => StateP String m (Method, ByteString, HTTPVersion)

requestLine :: (Monad m, Proxy p) => StateP [ByteString] p () (Maybe ByteString) y' y m (Maybe (Method, ByteString, HTTPVersion)) 
requestLine =
    do Just m <- method
       return $ Just (m, empty, HTTP11)
-}

{-
    do m <- method
       sp
       uri <- requestURI
       sp
       v <- httpVersion
       crlf
       return (m, uri, v)
-}

{-
sp :: (Monad m) => ParseT ProxyFast String m Char
sp = char ' '

crlf :: (Monad m) => ParseT ProxyFast String m String
crlf = string "\r\n"
o
requestLine :: (Monad m) => ParseT ProxyFast String m (Method, ByteString, HTTPVersion)
requestLine =
    do m <- method
       sp
       uri <- requestURI
       sp
       v <- httpVersion
       crlf
       return (m, uri, v)



{-

The Method token indicates the method to be performed on the resource identified by the Request-URI. The method is case-sensitive.

       Method         = "OPTIONS"                ; Section 9.2
                      | "GET"                    ; Section 9.3
                      | "HEAD"                   ; Section 9.4
                      | "POST"                   ; Section 9.5
                      | "PUT"                    ; Section 9.6
                      | "DELETE"                 ; Section 9.7
                      | "TRACE"                  ; Section 9.8
                      | "CONNECT"                ; Section 9.9
                      | extension-method
       extension-method = token
-}

method :: (Monad m) => ParseT ProxyFast String m Method
method =
         string "GET"     *> pure GET
     <|> string "POST"    *> pure POST
     <|> string "HEAD"    *> pure HEAD
     <|> string "OPTIONS" *> pure OPTIONS
     <|> string "PUT"     *> pure PUT
     <|> string "DELETE"  *> pure DELETE
     <|> string "CONNECT" *> pure CONNECT
     <|> ((EXTENSION . C.pack) <$> token)

{-
       token          = 1*<any CHAR except CTLs or separators>
       separators     = "(" | ")" | "<" | ">" | "@"
                      | "," | ";" | ":" | "\" | <">
                      | "/" | "[" | "]" | "?" | "="
                      | "{" | "}" | SP | HT
       CTL            = <any US-ASCII control character
                        (octets 0 - 31) and DEL (127)>
-}

token :: (Monad m) => ParseT ProxyFast String m String
token = some tokenChar

tokenChar :: (Monad m) => ParseT ProxyFast String m Char
tokenChar =
    do c <- draw
       if not (c `elem` notTokenChar)
          then return c
          else empty

notTokenChar :: [Char]
notTokenChar = separators ++ ctl

separators :: [Char]
separators = "()<>@,;:\\\"/[]?={} \t"

ctl :: [Char]
ctl = ['\0' .. '\31'] ++ ['\127']

requestURI :: (Monad m) => ParseT ProxyFast String m ByteString
requestURI = C.pack <$> some notSP
    where
      notSP =
          do c <- draw
             if c /= ' '
                then return c
                else empty

httpVersion :: (Monad m) => ParseT ProxyFast String m HTTPVersion
httpVersion =
    do string "HTTP/1."
       (char '1' >> return HTTP11) <|> (char '0' >> return HTTP10)

{-
       message-header = field-name ":" [ field-value ]
       field-name     = token
       field-value    = *( field-content | LWS )
       field-content  = <the OCTETs making up the field-value
                        and consisting of either *TEXT or combinations
                        of token, separators, and quoted-string>
-}

messageHeader :: (Monad m) => ParseT ProxyFast String m (ByteString, ByteString)
messageHeader =
    do fieldName <- token
       char ':'
       fieldValue <- some notCR
       crlf
       return (C.pack fieldName, C.pack fieldValue)
    where
      notCR =
          do c <- draw
             if c /= '\r'
                then return c
                else empty

------------------------------------------------------------------------------
-- Request Parser
------------------------------------------------------------------------------

{-
        Request       = Request-Line              ; Section 5.1
                        *(( general-header        ; Section 4.5
                         | request-header         ; Section 5.3
                         | entity-header ) CRLF)  ; Section 7.1
                        CRLF
                        [ message-body ]          ; Section 4.3
-}

pRequest :: (Monad m) =>
            Bool
         -> SockAddr
         -> ParseT ProxyFast String m Request
pRequest secure addr =
    do (m, u, v) <- requestLine
       headers <- many messageHeader
       crlf
       let req = Request { rqMethod      = m
                         , rqURIbs       = u
                         , rqHTTPVersion = v
                         , rqHeaders     = headers
                         , rqSecure      = secure
                         , rqClient      = addr
                         }
       return $! req

parseRequest' :: Monad m =>
                 Bool
              -> SockAddr
              -> (() -> Pipe ProxyFast (Maybe String) Request m ())
parseRequest' secure addr =
    evalParseT (pRequest secure addr)

-- FIXME: the 'return undefined' seems a bit wrong.
parseRequest :: Monad m =>
                Bool
             -> SockAddr
             -> (() -> Pipe ProxyFast ByteString b m Request)
parseRequest secure addr =
    onlyK (mapD C.unpack) >-> (\() -> parseRequest' secure addr () >> return undefined) >-> go
    where
      go :: (Monad m) => (() -> Pipe ProxyFast Request b m Request)
      go () =
          do r <- request ()
             return r

pr :: (Monad m) =>
      Bool
   -> SockAddr
   -> Consumer (ParseP String ProxyFast) (Maybe String) m Request
pr secure addr  = commit "whoa-mama" (pRequest secure addr)

pr' :: Monad m =>
       Bool
    -> SockAddr
    -> (() -> EitherP SomeException ProxyFast () (Maybe String) () C m Request)
pr' secure addr () = evalParseP $ pr secure addr


pr'' :: Monad m =>
        Bool
     -> SockAddr
     -> () -> Consumer ProxyFast (Maybe String) m (Either SomeException Request)
pr'' secure addr = runEitherK $ pr' secure addr

pr''' :: Monad m =>
        Bool
     -> SockAddr
     -> () -> Pipe ProxyFast ByteString c m (Either SomeException Request)
pr''' secure addr = onlyK (mapD C.unpack) >-> pr'' secure addr >-> unitU

ignoreMaybeString :: (Monad m) => () -> Pipe ProxyFast a b m r
ignoreMaybeString () =
    do ms  <- request ()
       ignoreMaybeString ()
{-
pr''' :: Monad m =>
         Bool
      -> SockAddr
      -> (() -> Consumer ProxyFast c m (Either SomeException Request))
pr''' secure addr = pr'' secure addr >-> ignoreMaybeString
-}

{-
------------------------------------------------------------------------------
-- 'Word8' constants for popular characters
------------------------------------------------------------------------------

colon, cr, nl, space :: Word8
colon = c2w ':'
cr    = c2w '\r'
nl    = c2w '\n'
space = c2w ' '

------------------------------------------------------------------------------
-- Parse Exception
------------------------------------------------------------------------------

data ParseError
    = Unexpected
    | MalformedRequestLine ByteString
    | MalformedHeader      ByteString
    | UnknownHTTPVersion   ByteString
      deriving (Typeable, Show, Eq)

instance Exception ParseError

------------------------------------------------------------------------------
-- Request Parser
------------------------------------------------------------------------------

{-
        Request       = Request-Line              ; Section 5.1
                        *(( general-header        ; Section 4.5
                         | request-header         ; Section 5.3
                         | entity-header ) CRLF)  ; Section 7.1
                        CRLF
                        [ message-body ]          ; Section 4.3
-}
parseRequest :: (Proxy p, Monad m) =>
                Bool -- ^ is this an HTTPS connection?
             -> SockAddr
             -> StateP ByteString p () ByteString a b m Request
parseRequest secure addr =
    do line <- takeLine
       let (method, requestURI, httpVersion) = parseRequestLine line
       headers <- parseHeaders
       let req =
               Request { rqMethod      = method
                       , rqURIbs       = requestURI
                       , rqHTTPVersion = httpVersion
                       , rqHeaders     = headers
                       , rqSecure      = secure
                       , rqClient      = addr
                       }
       return $! req

-- | currently if you consume the entire request body this will
-- terminate and return the 'ret' value that you supplied. But, that
-- seems wrong, because that will tear down the whole pipeline and
-- return that value instead of what you really wanted to return.
--
-- Perhaps this should return a 'Maybe ByteString' instead so you can
-- detect when the body ends? But that interfers with using
-- 'parseRequest' in 'httpPipe'. For now we will just return 'empty'
-- forever when you get to the end.
--
-- Perhaps pipes 2.5 will provide a better solution as it is supposed
-- to allow you to catch termination of the upstream pipe.
pipeBody :: (Proxy p, Monad m) =>
            Request
         -> ()
         -> StateP ByteString p () ByteString a ByteString m r
pipeBody req () =
    case lookup "Content-Length" (rqHeaders req) of
         Nothing ->
             do error "chunked bodies not supported yet"
         (Just value) ->
             case readDecimal (B.drop 1 value) of
               Nothing -> error $ "Failed to read Content-Length"
               (Just (n, _)) ->
                    do unconsumed <- get
                       go n unconsumed
    where
      go remaining unconsumed
          | remaining == 0 =
              do put unconsumed
                 done

          | remaining >= B.length unconsumed =
              do liftP $  respond unconsumed
                 bs <- liftP $ request ()
                 go (remaining - B.length unconsumed) bs

          | remaining == B.length unconsumed =
              do liftP $ respond unconsumed
                 put empty
                 done

          | otherwise =
              do let (bs', remainder) = B.splitAt remaining unconsumed
                 liftP $ respond bs'
                 put remainder
                 done

      done = forever $ liftP $ respond empty

{-
The Request-Line begins with a method token, followed by the Request-URI and the protocol version, and ending with CRLF. The elements are separated by SP characters. No CR or LF is allowed except in the final CRLF sequence.

        Request-Line   = Method SP Request-URI SP HTTP-Version CRLF
-}
parseRequestLine :: ByteString -> (Method, ByteString, HTTPVersion)
parseRequestLine bs =
    case split space bs of
      [method, requestURI, httpVersion] ->
          (parseMethod method, requestURI, parseHTTPVersion httpVersion)
      _ -> throw (MalformedRequestLine bs)


{-

The Method token indicates the method to be performed on the resource identified by the Request-URI. The method is case-sensitive.

       Method         = "OPTIONS"                ; Section 9.2
                      | "GET"                    ; Section 9.3
                      | "HEAD"                   ; Section 9.4
                      | "POST"                   ; Section 9.5
                      | "PUT"                    ; Section 9.6
                      | "DELETE"                 ; Section 9.7
                      | "TRACE"                  ; Section 9.8
                      | "CONNECT"                ; Section 9.9
                      | extension-method
       extension-method = token
-}

parseMethod :: ByteString -> Method
parseMethod bs
    | bs == "OPTIONS" = OPTIONS
    | bs == "GET"     = GET
    | bs == "HEAD"    = HEAD
    | bs == "POST"    = POST
    | bs == "PUT"     = PUT
    | bs == "DELETE"  = DELETE
    | bs == "TRACE"   = TRACE
    | bs == "CONNECT" = CONNECT
    | otherwise       = EXTENSION bs

parseHTTPVersion :: ByteString -> HTTPVersion
parseHTTPVersion bs
    | bs == "HTTP/1.1" = HTTP11
    | bs == "HTTP/1.0" = HTTP10
    | otherwise        = throw (UnknownHTTPVersion bs)

-- FIXME: add max header size checks
-- parseHeaders :: (Monad m) => ByteString -> Pipe ByteString b m ([(ByteString, ByteString)], ByteString)
parseHeaders :: (Proxy p, Monad m) => StateP ByteString p () ByteString a b m [(ByteString, ByteString)]
parseHeaders =
    do line <- takeLine
       if B.null line
          then do return []
          else do headers <- parseHeaders
                  return (parseHeader line : headers)


{-
       message-header = field-name ":" [ field-value ]
       field-name     = token
       field-value    = *( field-content | LWS )
       field-content  = <the OCTETs making up the field-value
                        and consisting of either *TEXT or combinations
                        of token, separators, and quoted-string>
-}

parseHeader :: ByteString -> (ByteString, ByteString)
parseHeader bs =
    let (fieldName, remaining) = parseToken bs
    in case uncons remaining of
         (Just (c, fieldValue))
             | c == colon -> (fieldName, fieldValue)
         _                -> throw (MalformedHeader bs)

{-
       token          = 1*<any CHAR except CTLs or separators>
       separators     = "(" | ")" | "<" | ">" | "@"
                      | "," | ";" | ":" | "\" | <">
                      | "/" | "[" | "]" | "?" | "="
                      | "{" | "}" | SP | HT
       CTL            = <any US-ASCII control character
                        (octets 0 - 31) and DEL (127)>
-}

-- FIXME: follow the spec
parseToken :: ByteString -> (ByteString, ByteString)
parseToken bs = B.span (/= colon) bs

-- | find a line terminated by a '\r\n'
takeLine :: (Proxy p, Monad m) =>
            StateP ByteString p () ByteString a b m ByteString
takeLine =
    do bs <- get
       case elemIndex nl bs of
         Nothing ->
             do x <- liftP $ request ()
                put (bs `mappend` x)
                takeLine
         (Just 0) -> throw Unexpected
         (Just i) ->
             if unsafeIndex bs (i - 1) /= cr
                then throw Unexpected
                else do put $ unsafeDrop (i + 1) bs
                        return $ unsafeTake (i - 1) bs

{-

parse :: (Monad m) => Pipe ByteString b m a -> String -> m (Maybe a)
parse parser str =
    runPipe $ (yield (C.pack str) >> return Nothing)
                >+> (fmap Just parser)
                >+> discard
-}
-}
-}-}