{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | The @Date@ field, formatted once a second at most.
module Hyperdrive.Server.Date
  ( DateCache
  , newDateCache
  , refreshDate
  , currentDate
  , formatHttpDate
  ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Int (Int64)
import Data.IORef
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Hyperdrive.Semantics.Field (FieldValue, mkFieldValue)

dateValue :: Int64 -> FieldValue
dateValue = either (error . show) id . mkFieldValue . formatHttpDate

-- | The current second, as a field value ready to send. One for the
-- whole server, brought up to date by the timers' ticker
-- ("Hyperdrive.Server.Timer") and only read by connections: reading the
-- clock for every response was one of three clock reads a request,
-- which together cost 0.09 microseconds of a small request's 2
-- (docs/benchmarks.md).
newtype DateCache = DateCache (IORef Dated)

data Dated = Dated !Int64 !FieldValue

newDateCache :: IO DateCache
newDateCache = do
  now <- systemSeconds <$> getSystemTime
  DateCache <$> (newIORef $! dated now)

dated :: Int64 -> Dated
dated now = let !v = dateValue now in Dated now v

-- | Read the clock, and format the date if the second has changed.
-- Only the ticker calls this.
refreshDate :: DateCache -> IO ()
refreshDate (DateCache ref) = do
  -- whole seconds directly: getPOSIXTime's fixed-point value would go
  -- through Integer arithmetic every time
  now <- systemSeconds <$> getSystemTime
  Dated second _ <- readIORef ref
  when (second /= now) (writeIORef ref $! dated now)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-6.6.1
-- # An origin server with a clock (as defined in Section 5.6.7) MUST
-- # generate a Date header field in all 2xx (Successful), 3xx
-- # (Redirection), and 4xx (Client Error) responses, and MAY generate a
-- # Date header field in 1xx (Informational) and 5xx (Server Error)
-- # responses.

-- | The time as an HTTP-date, as of the last 'refreshDate'.
currentDate :: DateCache -> IO FieldValue
currentDate (DateCache ref) = (\(Dated _ v) -> v) <$> readIORef ref

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.7
-- # IMF-fixdate  = day-name "," SP date1 SP time-of-day SP GMT

-- | Seconds since the epoch as an IMF-fixdate, the form senders must
-- use.
formatHttpDate :: Int64 -> ByteString
formatHttpDate t =
  C.pack (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" (posixSecondsToUTCTime (fromIntegral t)))
