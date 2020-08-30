{-# LANGUAGE OverloadedStrings #-}

{-|
Module: Data.Time.LocalTime.TimeZone.Detect
Portability: POSIX

Exposes utilities derived from the excellent ZoneDetect library <https://github.com/BertoldVdb/ZoneDetect>.
To use this module, you need to obtain database files from the aforementioned library's server.

Additionally, you can also derive a timezone at a point in space __and_time__ by using the *When variants.

Currently, only one function for looking up the name of a Timezone is provided, but the underlying
library also has richer functions for opening timezone database files, finding applicable zones, etc.
-}

module Data.Time.LocalTime.TimeZone.Detect 
    ( lookupTimeZoneName
    , timeAtPointToUTC
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


-- | Alias for clarity, timezones are short strings that follow the IANA conventions
-- documented here:
-- https://data.iana.org/time-zones/tz-link.html
type TimeZoneName = String

-- | Gets timezone info from the standard location in UNIX systems.
-- the name should be one of the standard tz database names.
-- See: <https://en.wikipedia.org/wiki/List_of_tz_database_time_zones>
getTimeZoneSeriesFromOlsonFileUNIX :: TimeZoneName -> IO TimeZoneSeries
getTimeZoneSeriesFromOlsonFileUNIX tzName =
    getTimeZoneSeriesFromOlsonFile $ "/usr/share/zoneinfo/" <> tzName

-- | Given a timezone database, latitude and longitude, try to determine the
-- timezone name. Follow the instructions in the C library's repository
-- to obtain timezone database files (<https://github.com/BertoldVdb/ZoneDetect/tree/05567e367576d7f3efa00083b7661a01e43dc8ca/database>)
-- Once in possesion of said files, the lookup looks as follows:
-- 
-- >>> tz <- lookupTimeZoneName "./test/tz_db/timezone21.bin" 40.7831 (-73.9712) :: Maybe TimeZoneName
-- Just "America/New_York"
-- 
-- Failure conditions are: invalid database file, or invalid coordinates,
-- both will cause the given monad to `fail`.
lookupTimeZoneName :: MonadFail m => FilePath -> Double -> Double -> m TimeZoneName
lookupTimeZoneName databaseLocation lat lng = 
    unsafePerformIO $ do
        zdPtr <- withCAString databaseLocation $ \dbl -> c_ZDOpenDatabase dbl
        if zdPtr == nullPtr then
            pure $ fail (databaseLocation <> " is not a valid timezone database.")
        else do
            tzName <- c_ZDHelperSimpleLookupString zdPtr
                                                   (realToFrac lat)
                                                   (realToFrac lng)
            if tzName == nullPtr then
                pure $ fail "Invalid coordinates."
            else
                peekCAString tzName >>= (pure . return)


-- | Given a timezone database, latitude, longitude and a local reference time, find the UTC Time
-- equivalent of that reference time in the given timezone. The reference time helps determine
-- which offset was in effect, since daylight savings, historical circumstances, political revisions
-- and other circumstances (documented in the olson tz database) may have been in effect
-- at that point in spacetime.
timeAtPointToUTC :: FilePath -> Double -> Double -> LocalTime -> IO UTCTime
timeAtPointToUTC databaseLocation lat lng referenceTime = do
    tzName <- lookupTimeZoneName databaseLocation lat lng
    tzSeries <- liftIO $ getTimeZoneSeriesFromOlsonFileUNIX tzName
    return $ localTimeToUTC' tzSeries referenceTime