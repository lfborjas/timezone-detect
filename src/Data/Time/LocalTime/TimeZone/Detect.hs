{-# LANGUAGE OverloadedStrings #-}

{-|
Module: Data.Time.LocalTime.TimeZone.Detect
Portability: POSIX

Exposes utilities derived from the excellent ZoneDetect library <https://github.com/BertoldVdb/ZoneDetect>.
To use this module, you need to obtain database files from the aforementioned library's server.

Additionally, if you have a local time, latitude and longitude, we use the timezone-series and timezone-olson
packages to help determine the equivalent UTC instant to that point in geography and time.

The only relevant binding to ZoneDetect is 'lookupTimeZoneName', richer information that plays neatly
with the notion of 'TimeZone' could be derived, but I didn't have a personal need for that yet.
-}

module Data.Time.LocalTime.TimeZone.Detect 
    ( lookupTimeZoneName
    , timeAtPointToUTC
    , timeInTimeZoneToUTC
    , getTimeZoneSeriesFromOlsonFileUNIX
    , TimeZoneName
) where

import Foreign.ZoneDetect
import Foreign.C.String (peekCAString, withCAString)
import Foreign (nullPtr)
import Data.Time
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Fail (MonadFail, fail)
import Prelude hiding (fail)

-- | Alias for clarity, timezones are path-like strings that follow the IANA conventions
-- documented here:
-- <https://data.iana.org/time-zones/tz-link.html>
-- and here:
-- <https://en.wikipedia.org/wiki/Tz_database#Names_of_time_zones>
type TimeZoneName = FilePath

-- | Gets timezone info from the standard location in UNIX systems.
-- The name should be one of the standard tz database names, as returned
-- by `lookupTimeZoneName`.
-- See: <https://en.wikipedia.org/wiki/List_of_tz_database_time_zones>
getTimeZoneSeriesFromOlsonFileUNIX :: TimeZoneName -> IO TimeZoneSeries
getTimeZoneSeriesFromOlsonFileUNIX tzName =
    getTimeZoneSeriesFromOlsonFile $ "/usr/share/zoneinfo/" ++ tzName

-- | Given a timezone database, latitude and longitude, try to determine the
-- timezone name. Follow the instructions in the C library's repository
-- to obtain timezone database files (<https://github.com/BertoldVdb/ZoneDetect/tree/05567e367576d7f3efa00083b7661a01e43dc8ca/database>)
-- Once in possesion of said files, the lookup looks as follows:
-- 
-- >>> tz <- lookupTimeZoneName "./test/tz_db/timezone21.bin" 40.7831 (-73.9712) :: Maybe TimeZoneName
-- Just "America/New_York"
-- 
-- An invalid database file, or invalid coordinates, will cause the given monad to `fail`.
lookupTimeZoneName :: MonadFail m => FilePath -> Double -> Double -> m TimeZoneName
lookupTimeZoneName databaseLocation lat lng = 
    unsafePerformIO $ do
        zdPtr <- withCAString databaseLocation $ \dbl -> c_ZDOpenDatabase dbl
        if zdPtr == nullPtr then
            pure $ fail (databaseLocation ++ " is not a valid timezone database.")
        else do
            tzName <- c_ZDHelperSimpleLookupString zdPtr
                                                   (realToFrac lat)
                                                   (realToFrac lng)
            if tzName == nullPtr then
                pure $ fail "Invalid coordinates."
            else
                peekCAString tzName >>= (pure . return)


-- | Given a timezone name (presumably obtained via `lookupTimeZoneName`,)
-- and a reference time in `LocalTime`, find the UTC equivalent.
timeInTimeZoneToUTC :: TimeZoneName -> LocalTime -> IO UTCTime
timeInTimeZoneToUTC tzName referenceTime = do
    tzSeries <- getTimeZoneSeriesFromOlsonFileUNIX tzName
    return $ localTimeToUTC' tzSeries referenceTime

-- | Given a timezone database, latitude, longitude and a local reference time, find the UTC Time
-- equivalent of that reference time in the given timezone. The reference time helps determine
-- which offset was in effect, since daylight savings, historical circumstances, political revisions
-- and other circumstances (documented in the olson tz database) may have been in effect
-- at that point in spacetime.
timeAtPointToUTC :: FilePath -> Double -> Double -> LocalTime -> IO UTCTime
timeAtPointToUTC databaseLocation lat lng referenceTime = do
    tzName <- lookupTimeZoneName databaseLocation lat lng
    timeInTimeZoneToUTC tzName referenceTime
