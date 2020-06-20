module Types(
              Environment(..)
            , Coordinate(..)
            , Detector(..)
            , DetectionProbability(..)
            ) where

data Environment = Environment { roomSize :: Float, detectors :: [Detector] } deriving (Eq, Show)

newtype Detector = Detector { coordinate :: Coordinate } deriving (Eq, Show)

data Coordinate = Coordinate Float Float deriving (Eq, Ord, Show)

newtype DetectionProbability = DetectionProbability { detectionProbability :: Float } deriving (Eq, Ord, Show)
