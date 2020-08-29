module TimezoneDetect where

import Foreign.ZoneDetect
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (peekCAString, withCAString)
import Foreign (nullPtr)

type TimeZoneName = String

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
