module TimezoneDetectSpec (spec) where

import Data.Time
import Data.Time.LocalTime.TimeZone.Detect
import Test.Hspec
import Test.QuickCheck (oneof, choose, forAll, Gen)
import Data.Maybe (isNothing, isJust)

zoneFile :: FilePath
zoneFile = "./test/tz_db/timezone21.bin"

localTimeFromString :: String -> IO LocalTime
localTimeFromString = 
    parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T"

utcFromString :: String -> IO UTCTime
utcFromString =
    parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T"

spec :: Spec
spec = do
    -- Simplistic functions: only work in IO, need a path to the TZ file, manage resources on their own.
    describe "lookupTimeZoneNameFromFile" $ do
        it "doesn't need a timezone db, just a path to the file, to detect the timezone." $ do
            newYork <- lookupTimeZoneNameFromFile zoneFile 40.7831 (-73.9712)
            newYork `shouldBe` "America/New_York"

    describe "timeAtPointToUTCFromFile" $ do
        it "doesn't need a timezone db, just a path to the file, to detect the UTC instant in a point and time" $ do
            localSummer <- localTimeFromString "2019-08-25 00:30:00"
            utcSummer   <- utcFromString "2019-08-25 04:30:00"
            atPointSummer <- timeAtPointToUTCFromFile zoneFile 40.7831 (-73.9712) localSummer

            atPointSummer `shouldBe` utcSummer
    
    describe "timeInTimeZoneToUTC" $ do
        it "calculates a UTC instant given a timezone name" $ do
            localWinter <- localTimeFromString "2019-12-25 00:30:00"
            localSummer <- localTimeFromString "2019-08-25 00:30:00"
            utcWinter <- utcFromString "2019-12-25 05:30:00"
            utcSummer <- utcFromString "2019-08-25 04:30:00"
            alwaysSummer  <- utcFromString "2019-08-25 06:30:00"

            atTZWinter <- timeInTimeZoneToUTC "America/New_York" localWinter
            atTZSummer <- timeInTimeZoneToUTC "America/New_York" localSummer
            atTZNoDST  <- timeInTimeZoneToUTC "America/Tegucigalpa" localSummer

            atTZWinter `shouldBe` utcWinter
            atTZSummer `shouldBe` utcSummer
            atTZNoDST  `shouldBe` alwaysSummer

    -- More general functions: work with a reference to an open TZ DB. You can use `withTimeZoneDatabase`
    -- for resource management (opens the file, does the computation, closes.)
    around (withTimeZoneDatabase zoneFile) $ do
        describe "lookupTimeZoneName" $ do
            it "returns the expected timezones for known locations" $ \db -> do
                let newYork = lookupTimeZoneName db 40.7831 (-73.9712)
                    tegucigalpa = lookupTimeZoneName db 14.0650 (-87.1715)
                newYork `shouldBe` (Just "America/New_York")
                tegucigalpa `shouldBe` (Just "America/Tegucigalpa")

            it "returns an error value when given invalid coordinates" $ \db -> do
                let outOfThisWorld = lookupTimeZoneName db 40000000.7 (-73.97)
                outOfThisWorld `shouldBe` Nothing -- invalid coordinates

            it "returns an error value when given a bogus database" $ \_ -> do
                badDb <- openTimeZoneDatabase "bogus"
                let bogus = lookupTimeZoneName badDb 40.7831 (-73.9712)
                bogus `shouldBe` Nothing

            it "finds a timezone name for any latitude and longitude on Earth" $ \db -> do
                forAll genCoords $ \(a,b) ->
                    isJust $ lookupTimeZoneName db a b

            it "fails for any invalid set of coordinates" $ \db -> do
                forAll genBadCoords $ \(a,b) ->
                    isNothing $ lookupTimeZoneName db a b

        describe "timeAtPointToUTC" $ do
            it "calculates a UTC instant at a point in time and space in New York" $ \db -> do
                localWinter <- localTimeFromString "2019-12-25 00:30:00"
                localSummer <- localTimeFromString "2019-08-25 00:30:00"
                utcWinter <- utcFromString "2019-12-25 05:30:00"
                utcSummer <- utcFromString "2019-08-25 04:30:00"

                atPointWinter <- timeAtPointToUTC db 40.7831 (-73.9712) localWinter
                atPointSummer <- timeAtPointToUTC  db 40.7831 (-73.9712) localSummer

                atPointWinter `shouldBe` utcWinter
                atPointSummer `shouldBe` utcSummer

            it "calculates a UTC instant at a point in time and space in Tegucigalpa (no DST)" $ \db -> do
                localWinter <- localTimeFromString "2019-12-25 00:30:00"
                localSummer <- localTimeFromString "2019-08-25 00:30:00"
                utcWinter <- utcFromString "2019-12-25 06:30:00"
                utcSummer <- utcFromString "2019-08-25 06:30:00"

                atPointWinter <- timeAtPointToUTC db 14.0650 (-87.1715) localWinter
                atPointSummer <- timeAtPointToUTC db 14.0650 (-87.1715) localSummer

                atPointWinter `shouldBe` utcWinter
                atPointSummer `shouldBe` utcSummer

genCoords :: Gen (Double, Double)
genCoords = do
    lat <- choose (-90.0, 90.0)
    lng <- choose (-180.0, 180.0)
    return (lat, lng)

genBadCoords :: Gen (Double, Double)
genBadCoords = do
    lat <- oneof [choose (-360, -90), choose (90, 360)]
    lng <- oneof [choose (-360, -180), choose (180, 360)]
    return (lat, lng)
