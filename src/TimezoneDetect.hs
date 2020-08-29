module TimezoneDetect where

import Foreign.ZoneDetect
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (peekCAString, withCAString)

type TimeZoneName = String

lookupTimeZone :: FilePath -> Float -> Float -> TimeZoneName
lookupTimeZone databaseLocation lat lng =
    unsafePerformIO $ do
        zdPtr <- withCAString databaseLocation $ \dbl -> c_ZDOpenDatabase dbl
        tzName <- c_ZDHelperSimpleLookupString zdPtr
                                               (realToFrac lat)
                                               (realToFrac lng)
        peekCAString tzName
