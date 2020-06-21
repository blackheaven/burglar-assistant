module Types(
              Environment(..)
            , Coordinate(..)
            , Detector(..)
            , DetectionProbability(..)
            ) where

data Environment = Environment { roomSizeHeight :: Float, roomSizeWidth :: Float, detectors :: [Detector] } deriving (Eq, Show)

data Detector = Detector { coordinate :: Coordinate, power :: Float } deriving (Eq, Show)

data Coordinate = Coordinate Float Float deriving (Eq, Ord, Show)

newtype DetectionProbability = DetectionProbability { detectionProbability :: Float } deriving (Eq, Ord, Show)
