{-|
Module: TimezoneDetect
Portability: POSIX

Exposes utilities derived from the excellent ZoneDetect library <https://github.com/BertoldVdb/ZoneDetect>.
To use this module, you need to obtain database files from the aforementioned library's server.

Currently, only one function for looking up the name of a TimeZone is provided, but the underlying
library also has richer functions for opening timezone database files, finding applicable zones, etc.
-}

module TimezoneDetect where

import Foreign.ZoneDetect
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (peekCAString, withCAString)
import Foreign (nullPtr)

-- | Alias for clarity, timezones are short strings that follow the IANA conventions
-- documented here:
-- https://data.iana.org/time-zones/tz-link.html
type TimeZoneName = String

-- | Given a timezone database, latitude and longitude, try to determine the
-- timezone name. Follow the instructions in the C library's repository
-- to obtain timezone database files (https://github.com/BertoldVdb/ZoneDetect/tree/05567e367576d7f3efa00083b7661a01e43dc8ca/database)
-- Once in possesion of said files, the lookup looks as follows:
-- >>> lookupTimeZone "./test/tz_db/timezone21.bin" 40.7831 (-73.9712)
-- Right "America/New_York"
-- Failure conditions are: invalid database file, or invalid coordinates,
-- both are returned as `Left` values.
lookupTimeZone :: FilePath -> Float -> Float -> Either String TimeZoneName
lookupTimeZone databaseLocation lat lng =
    unsafePerformIO $ do
        zdPtr <- withCAString databaseLocation $ \dbl -> c_ZDOpenDatabase dbl
        if zdPtr == nullPtr then
            pure $ Left (databaseLocation <> " is not a valid timezone database.")
        else do
            tzName <- c_ZDHelperSimpleLookupString zdPtr
                                                   (realToFrac lat)
                                                   (realToFrac lng)
            if tzName == nullPtr then
                pure $ Left "Invalid coordinates."
            else
                peekCAString tzName >>= (pure . Right)
