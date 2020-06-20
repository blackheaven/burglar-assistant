module IOSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import IO
import Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "printRounded" $ do
    it "exact number zero should not have trailing zeros" $ do
      printRounded (DetectionProbability 0) `shouldBe` "0"
    it "exact number short should not change" $ do
      printRounded (DetectionProbability 0.13) `shouldBe` "0.13"
    it "exact number should not change" $ do
      printRounded (DetectionProbability 0.123) `shouldBe` "0.123"
    it "under 0.5 should round down" $ do
      printRounded (DetectionProbability 0.253499) `shouldBe` "0.253"
    it "above 0.5 should round up" $ do
      printRounded (DetectionProbability 0.1295) `shouldBe` "0.13"
  describe "parseMapFile" $ do
    it "correct values should have the exact environment" $ do
      parseMapFile (unlines ["10", "2", "2.42 5.26", "0 7"]) `shouldBe` Just (Environment 10.0 [Detector (Coordinate 2.42 5.26), Detector (Coordinate 0 7)])
    it "not enough detectors should be Nothing" $ do
      parseMapFile (unlines ["10", "3", "2.42 5.26", "0 7"]) `shouldBe` Nothing
    it "too much detectors should be Nothing" $ do
      parseMapFile (unlines ["10", "3", "2.42 5.26", "0 7"]) `shouldBe` Nothing
    it "detectors above the room size should be nothing" $ do
      parseMapFile (unlines ["15.0", "2", "2.42 5.26", "0 17"]) `shouldBe` Nothing
    it "the room size is equals to zero should be nothing" $ do
      parseMapFile (unlines ["0.0", "2", "0 0", "0 0"]) `shouldBe` Nothing
