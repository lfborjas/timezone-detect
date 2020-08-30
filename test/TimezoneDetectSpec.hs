module TimezoneDetectSpec (spec) where

import Data.Time
import Data.Time.LocalTime.TimeZone.Detect
import Test.Hspec

zoneFile :: FilePath
zoneFile = "./test/tz_db/timezone21.bin"

lookupTimeZoneName' :: Double -> Double -> Maybe TimeZoneName
lookupTimeZoneName' = lookupTimeZoneName zoneFile

localTimeFromString :: String -> IO LocalTime
localTimeFromString = 
    parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T"

utcFromString :: String -> IO UTCTime
utcFromString =
    parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T"

timeAtPointToUTC' :: Double -> Double -> LocalTime -> IO UTCTime
timeAtPointToUTC' = timeAtPointToUTC zoneFile

spec :: Spec
spec = do
    describe "lookupTimeZoneName" $ do
        it "calculates coordinates for known locations" $ do
            let newYork = lookupTimeZoneName' 40.7831 (-73.9712)
            let tegucigalpa = lookupTimeZoneName' 14.0650 (-87.1715)
            newYork `shouldBe` (Just "America/New_York")
            tegucigalpa `shouldBe` (Just "America/Tegucigalpa")

        it "returns an error value when given an invalid timezone file path" $ do
            let wrong = lookupTimeZoneName "bogus" 0.0 0.0
            wrong `shouldBe` Nothing -- invalid DB file

        it "returns an error value when given invalid coordinates" $ do
            let outOfThisWorld = lookupTimeZoneName' 40000000.7 (-73.97)
            outOfThisWorld `shouldBe` Nothing -- invalid coordinates

    describe "timeAtPointToUTC" $ do
        it "calculates a UTC instant at a point in time and space in New York" $ do
            localWinter <- localTimeFromString "2019-12-25 00:30:00"
            localSummer <- localTimeFromString "2019-08-25 00:30:00"
            utcWinter <- utcFromString "2019-12-25 05:30:00"
            utcSummer <- utcFromString "2019-08-25 04:30:00"
            
            atPointWinter <- timeAtPointToUTC' 40.7831 (-73.9712) localWinter
            atPointSummer <- timeAtPointToUTC' 40.7831 (-73.9712) localSummer

            atPointWinter `shouldBe` utcWinter
            atPointSummer `shouldBe` utcSummer

        it "calculates a UTC instant at a point in time and space in Tegucigalpa (no DST)" $ do
            localWinter <- localTimeFromString "2019-12-25 00:30:00"
            localSummer <- localTimeFromString "2019-08-25 00:30:00"
            utcWinter <- utcFromString "2019-12-25 06:30:00"
            utcSummer <- utcFromString "2019-08-25 06:30:00"
            
            atPointWinter <- timeAtPointToUTC' 14.0650 (-87.1715) localWinter
            atPointSummer <- timeAtPointToUTC' 14.0650 (-87.1715) localSummer

            atPointWinter `shouldBe` utcWinter
            atPointSummer `shouldBe` utcSummer
