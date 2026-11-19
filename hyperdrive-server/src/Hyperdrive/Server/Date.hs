{-# LANGUAGE OverloadedStrings #-}
-- | The @Date@ field, formatted once a second at most.
module Hyperdrive.Server.Date
  ( DateCache
  , newDateCache
  , currentDate
  , formatHttpDate
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Int (Int64)
import Data.IORef
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)

-- | The last second formatted, and its text. Each connection keeps its
-- own, so nothing is shared between threads.
newtype DateCache = DateCache (IORef (Int64, ByteString))

newDateCache :: IO DateCache
newDateCache = DateCache <$> newIORef (-1, "")

-- = https://www.rfc-editor.org/rfc/rfc9110#section-6.6.1
-- # An origin server with a clock (as defined in Section 5.6.7) MUST
-- # generate a Date header field in all 2xx (Successful), 3xx
-- # (Redirection), and 4xx (Client Error) responses, and MAY generate a
-- # Date header field in 1xx (Informational) and 5xx (Server Error)
-- # responses.

-- | The current time as an HTTP-date.
currentDate :: DateCache -> IO ByteString
currentDate (DateCache ref) = do
  -- whole seconds directly: getPOSIXTime's fixed-point value would go
  -- through Integer arithmetic on every response
  now <- systemSeconds <$> getSystemTime
  (second, text) <- readIORef ref
  if second == now then pure text else do
    let text' = formatHttpDate now
    writeIORef ref (now, text')
    pure text'

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.7
-- # IMF-fixdate  = day-name "," SP date1 SP time-of-day SP GMT

-- | Seconds since the epoch as an IMF-fixdate, the form senders must
-- use.
formatHttpDate :: Int64 -> ByteString
formatHttpDate t =
  C.pack (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" (posixSecondsToUTCTime (fromIntegral t)))
