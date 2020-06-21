module Assistant(
                  findLowestDetectionProbability
                , computeDetectionProbability
                , computeCoordinateDistance
                ) where

import Data.List(minimumBy, partition)
import qualified Data.Map.Lazy as M
import Data.Maybe(catMaybes)
import Data.Ord(comparing)
import Graph.DijkstraSimple

import Types

computeDetectionProbability :: Float -> Detector -> Coordinate -> DetectionProbability
computeDetectionProbability roomSize (Detector d) t = DetectionProbability $ exp (- (pi * (computeCoordinateDistance d t) /roomSize) ** 2)

computeCoordinateDistance :: Coordinate -> Coordinate -> Float
computeCoordinateDistance (Coordinate ax ay) (Coordinate bx by) = sqrt $ (ax - bx) ** 2 + (ay - by) ** 2

findLowestDetectionProbability :: Environment -> DetectionProbability
findLowestDetectionProbability (Environment roomSize detectors) = pathWeight $ (pathsAsMap paths) M.! (Coordinate (roomSize - stepSize) $ roomSize/2)
  where stepSize = roomSize / 10
        parts =  [0, stepSize..roomSize - stepSize]
        squaresMap = buildSquares roomSize stepSize detectors $ concatMap (\x -> map (Coordinate x) parts) parts
        weighter = Weighter (DetectionProbability 0) $ \e p -> max (pathWeight p) (edgeToWeight e)
        graph = buildGraph stepSize squaresMap
        paths = shortestPaths graph (Coordinate 0 $ roomSize/2) weighter

data Square = Square { leftDownCorner :: Coordinate, detectionProbability :: DetectionProbability }

instance Show Square where
  show (Square (Coordinate x y) (DetectionProbability p)) = show (x, y) ++ " -> " ++ show p

buildSquares :: Float -> Float -> [Detector] -> [Coordinate] -> M.Map Coordinate Square
buildSquares roomSize squareSize detectors = M.fromList . map (\c -> (c, buildSquare c))
  where buildSquare c = Square c (findLowestProbability c)
        findLowestProbability :: Coordinate -> DetectionProbability
        findLowestProbability c
         | null inside = maximum $ map (\d -> computeDetectionProbability roomSize d c) detectors
         | otherwise   = DetectionProbability 1
          where (inside, outside) = partition (isInSquare c . coordinate) detectors
                isInSquare (Coordinate ax ay) (Coordinate bx by) = ax <= bx && (ax + squareSize) >= bx && ay <= by && (ay + squareSize) >= by

buildGraph :: Float -> M.Map Coordinate Square -> Graph Coordinate DetectionProbability
buildGraph squareSize squaresMap = Graph $ M.fromList $ map buildVertice $ M.keys squaresMap
  where buildVertice o@(Coordinate x y) =
          (o, catMaybes [ (\(Square c d) -> EdgeTo c d) <$> (squaresMap M.!? Coordinate x'' y'')
                        | (x', y') <- [
                                        (squareSize, squareSize)
                                      , (squareSize, 0)
                                      , (squareSize, - squareSize)
                                      , (0, - squareSize)
                                      , (- squareSize, - squareSize)
                                      , (- squareSize, 0)
                                      , (- squareSize, - squareSize)
                                      ]
                        , (x'', y'') <- [(x + x', y + y')]
                        ])
