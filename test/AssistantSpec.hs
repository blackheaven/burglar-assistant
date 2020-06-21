module AssistantSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Assistant
import Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "computeCoordinateDistance" $ do
    it "a simple case" $
      computeCoordinateDistance (Coordinate 0 0) (Coordinate 3 4) `shouldBe` 5.0
    it "is always positive" $ property $
      \x y -> (computeCoordinateDistance x y >= 0) === True
  describe "computeDetectionProbability" $ do
    it "a simple case" $
      computeDetectionProbability (Detector (Coordinate 0 0) 5.0) (Coordinate 2 3) `shouldBe` DetectionProbability 0.59452057
    it "is always between 0 and 1" $ property $
      \x y p -> p > 1 ==> let r = computeDetectionProbability (Detector x p) y in (r >= DetectionProbability 0 && r <= DetectionProbability 1) === True
    it "is dependant of the distance" $ property $
      \x y z p -> p > 1 ==> (computeDetectionProbability (Detector x p) z `compare` computeDetectionProbability (Detector y p) z) `isOpposed` (computeCoordinateDistance x z `compare` computeCoordinateDistance y z) === True
  describe "findLowestDetectionProbability" $
    it "the example case" $
      findLowestDetectionProbability exampleEnvironment `shouldBe` DetectionProbability 0.4347347

exampleEnvironment :: Environment
exampleEnvironment = Environment 25.0 25.0 [
  Detector (Coordinate 2.423929917008996 20.187139309438546) 5,
  Detector (Coordinate 19.39788132776695 14.174570106439353) 5,
  Detector (Coordinate 1.3175678970133191 10.019351994529405) 5,
  Detector (Coordinate 1.0536920857525445 2.8936703202385115) 5,
  Detector (Coordinate 15.739302303324447 15.87541372165791) 5]

findRoomSize :: Float -> [Coordinate] -> Float
findRoomSize roomSize coordinates = maximum $ roomSize:map (\(Coordinate x y) -> max x y) coordinates

instance Arbitrary Coordinate where
  arbitrary = Coordinate <$> choose (0.0, 10.0) <*> choose (0.0, 10.0)
  shrink (Coordinate x y) = uncurry Coordinate <$> shrink (x, y)

isOpposed :: Ordering -> Ordering -> Bool
isOpposed x y = (x == y && x == EQ) || (x /= EQ && y /= EQ && x /= y)
