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

Functions to open, close, and work with an open database (from a valid file) are provided.
-}

module Data.Time.LocalTime.TimeZone.Detect 
    ( TimeZoneName
    , TimeZoneDatabase
    -- for managing the database info pointer:
    , openTimeZoneDatabase
    , closeTimeZoneDatabase
    , withTimeZoneDatabase
    -- timezone lookup:
    , lookupTimeZoneName
    , lookupTimeZoneNameFromFile
    -- (local time, place) -> UTC "instant"
    , timeAtPointToUTC
    , timeAtPointToUTCFromFile
    -- (local time, timezone name) -> UTC "instant"
    , timeInTimeZoneToUTC
) where

import Foreign.ZoneDetect
import Foreign.C.String (peekCAString, withCAString)
import Foreign (Ptr, nullPtr)
import Data.Time
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Fail (MonadFail, fail)
import Prelude hiding (fail)
import Control.Exception (bracket)

-- | Alias for clarity, timezones are path-like strings that follow the IANA conventions
-- documented here:
-- <https://data.iana.org/time-zones/tz-link.html>
-- and here:
-- <https://en.wikipedia.org/wiki/Tz_database#Names_of_time_zones>
type TimeZoneName = FilePath

-- | A reference to a timezone database.
type TimeZoneDatabase = Ptr ZoneDetectInfo

-- | Given a timezone database, latitude and longitude, try to determine the
-- timezone name. Follow the instructions in the C library's repository
-- to obtain timezone database files (<https://github.com/BertoldVdb/ZoneDetect/tree/05567e367576d7f3efa00083b7661a01e43dc8ca/database>)
-- Once in possesion of said files, the lookup looks as follows:
-- 
-- >>> db <- openTimeZoneDatabase "./test/tz_db/timezone21.bin" 
-- >>> let tz = lookupTimeZoneName db 40.7831 (-73.9712) :: Maybe TimeZoneName
-- Just "America/New_York"
-- >>> closeTimeZoneDatabase db
-- 
-- An invalid database pointer, or invalid coordinates, will cause the given monad to `fail`.
-- Note that we follow the original library's pattern of obtaining the `TimeZoneDatabase`
-- separately (which could prove advantageous in e.g a web server.) If you're doing
-- a one-off lookup, or are okay with the IO cost of opening the DB file, see
-- `lookupTimeZoneNameFromFile`.
lookupTimeZoneName :: MonadFail m => TimeZoneDatabase -> Double -> Double -> m TimeZoneName
lookupTimeZoneName database lat lng =
    unsafePerformIO $ do
        if database == nullPtr then
            pure $ fail "Invalid timezone database."
        else do
            tzName <- c_ZDHelperSimpleLookupString database
                                                   (realToFrac lat)
                                                   (realToFrac lng)
            if tzName == nullPtr then
                pure $ fail "Invalid coordinates."
            else
                peekCAString tzName >>= (pure . return)

-- | Same as `lookupTimeZoneName`, but takes the path to the database file and only works in `IO`.
lookupTimeZoneNameFromFile :: FilePath -> Double -> Double -> IO TimeZoneName
lookupTimeZoneNameFromFile databaseLocation lat lng =
    withTimeZoneDatabase databaseLocation
        (\db -> lookupTimeZoneName db lat lng) 

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
timeAtPointToUTC :: TimeZoneDatabase -> Double -> Double -> LocalTime -> IO UTCTime
timeAtPointToUTC database lat lng referenceTime = do
    tzName <- lookupTimeZoneName database lat lng
    timeInTimeZoneToUTC tzName referenceTime

-- | Same as `timeAtPointToUTC`, but takes the path to the timezone database file
-- and takes care of opening and closing the file.
timeAtPointToUTCFromFile :: FilePath -> Double -> Double -> LocalTime -> IO UTCTime
timeAtPointToUTCFromFile databaseLocation lat lng referenceTime =
    withTimeZoneDatabase databaseLocation
        (\db -> timeAtPointToUTC db lat lng referenceTime) 

-- | Gets timezone info from the standard location in UNIX systems.
-- The name should be one of the standard tz database names, as returned
-- by `lookupTimeZoneName`.
-- See: <https://en.wikipedia.org/wiki/List_of_tz_database_time_zones>
getTimeZoneSeriesFromOlsonFileUNIX :: TimeZoneName -> IO TimeZoneSeries
getTimeZoneSeriesFromOlsonFileUNIX tzName =
    getTimeZoneSeriesFromOlsonFile $ "/usr/share/zoneinfo/" ++ tzName

-- | Open a timezone database file and obtain a pointer to the database,
-- as interpreted by the underlying C code.
openTimeZoneDatabase :: FilePath -> IO TimeZoneDatabase
openTimeZoneDatabase databaseLocation = 
    withCAString databaseLocation $ \dbl -> c_ZDOpenDatabase dbl

-- | Given a pointer to a timezone database, close any allocated resources.
closeTimeZoneDatabase :: TimeZoneDatabase -> IO ()
closeTimeZoneDatabase = c_ZDCloseDatabase

-- | Given a path to a timezone database file, and a computation to run with it,
-- takes care of opening the file, running the computation and then closing it.
withTimeZoneDatabase :: FilePath -> (TimeZoneDatabase -> IO a) -> IO a
withTimeZoneDatabase databaseLocation = 
    bracket (openTimeZoneDatabase databaseLocation)
            (closeTimeZoneDatabase)
