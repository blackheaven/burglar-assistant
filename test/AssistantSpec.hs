module AssistantSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Assistant
import IO
import Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "computeCoordinateDistance" $ do
    it "a simple case" $ do
      computeCoordinateDistance (Coordinate 0 0) (Coordinate 3 4) `shouldBe` 5.0
    it "is always positive" $ property $
      \x y -> (computeCoordinateDistance x y >= 0) === True
  describe "computeDetectionProbability" $ do
    it "a simple case" $ do
      computeDetectionProbability 5.0 (Detector $ Coordinate 0 0) (Coordinate 2 3) `shouldBe` DetectionProbability 0.005903591
    it "is always between 0 and 1" $ property $
      \x y s -> let r = computeDetectionProbability' s (Detector x) y in (r >= DetectionProbability 0 && r <= DetectionProbability 1) === True
    it "is dependant of the distance" $ property $
      \x y z s -> s > 0 ==> let s' = findRoomSize s [x, y, z] in (computeDetectionProbability s' (Detector x) z `compare` computeDetectionProbability s' (Detector y) z) `isOpposed` (computeCoordinateDistance x z `compare` computeCoordinateDistance y z) === True
  describe "findLowestDetectionProbability" $ do
    it "the example case" $ do
      printRounded (findLowestDetectionProbability exampleEnvironment) `shouldBe` "0.245"

exampleEnvironment :: Environment
exampleEnvironment = Environment 25.0 [
  Detector (Coordinate 2.423929917008996 20.187139309438546),
  Detector (Coordinate 19.39788132776695 14.174570106439353),
  Detector (Coordinate 1.3175678970133191 10.019351994529405),
  Detector (Coordinate 1.0536920857525445 2.8936703202385115),
  Detector (Coordinate 15.739302303324447 15.87541372165791)]

computeDetectionProbability' :: Float -> Detector -> Coordinate -> DetectionProbability
computeDetectionProbability' roomSize d@(Detector (Coordinate a b)) t@(Coordinate x y) = computeDetectionProbability (maximum [roomSize, a, b, x, y]) d t

findRoomSize :: Float -> [Coordinate] -> Float
findRoomSize roomSize coordinates = maximum $ roomSize:map (\(Coordinate x y) -> max x y) coordinates

instance Arbitrary Coordinate where
  arbitrary = Coordinate <$> choose (0.0, 10.0) <*> choose (0.0, 10.0)
  shrink (Coordinate x y) = uncurry Coordinate <$> shrink (x, y)

isOpposed :: Ordering -> Ordering -> Bool
isOpposed x y = (x == y && x == EQ) || (x /= EQ && y /= EQ && x /= y)
