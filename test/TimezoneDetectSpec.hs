module TimezoneDetectSpec (spec) where

import TimezoneDetect
import Test.Hspec

zoneFile :: FilePath
zoneFile = "./test/tz_db/timezone21.bin"

lookupTimeZone' :: Float -> Float -> Either String TimeZoneName
lookupTimeZone' = lookupTimeZone zoneFile

spec :: Spec
spec = do
    describe "lookupTimeZone" $ do
        it "calculates coordinates for known locations" $ do
            let newYork = lookupTimeZone' 40.7831 (-73.9712)
            let tegucigalpa = lookupTimeZone' 14.0650 (-87.1715)
            newYork `shouldBe` (Right "America/New_York")
            tegucigalpa `shouldBe` (Right "America/Tegucigalpa")

        it "returns an error value when given an invalid timezone file path" $ do
            let wrong = lookupTimeZone "bogus" 0.0 0.0
            wrong `shouldBe` (Left "bogus is not a valid timezone database.")

        it "returns an error value when given invalid coordinates" $ do
            let outOfThisWorld = lookupTimeZone' 40000000.7 (-73.97)
            outOfThisWorld `shouldBe` (Left "Invalid coordinates.")
