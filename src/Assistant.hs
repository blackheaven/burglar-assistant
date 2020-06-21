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

computeDetectionProbability :: Detector -> Coordinate -> DetectionProbability
computeDetectionProbability (Detector d p) t = DetectionProbability $ exp (- (computeCoordinateDistance d t) ** 2 / (p ** 2))

computeCoordinateDistance :: Coordinate -> Coordinate -> Float
computeCoordinateDistance (Coordinate ax ay) (Coordinate bx by) = sqrt $ (ax - bx) ** 2 + (ay - by) ** 2

findLowestDetectionProbability :: Environment -> DetectionProbability
findLowestDetectionProbability env@(Environment roomSizeHeight roomSizeWidth detectors) = maximum $ map (pathWeight . (pathsAsMap paths M.!)) [enter, exit]
  where lowestProbability = findLowestProbability (roomSizeHeight/100) (roomSizeWidth/100) detectors
        rectanglesMap = buildRectangles lowestProbability $ splitRoom 100 env
        weighter = Weighter (DetectionProbability 0) $ \e p -> max (pathWeight p) (edgeToWeight e)
        graph = buildGraph rectanglesMap
        (enter, target, exit) = ((0, 99), (50, 50), (99, 0))
        paths = shortestPaths graph target weighter

data Rectangle = Rectangle { leftDownCorner :: Coordinate, detectionProbability :: DetectionProbability }
type RectangleCoordinate = (Int, Int)

-- | Split the room in multiple rectangles in order to approximate global
-- probability.
-- The first parameter indicate the number of split per height and width,
-- meaning the number of rectangle will be squarred this number.
splitRoom :: Int -> Environment -> [(RectangleCoordinate, Coordinate)]
splitRoom parts (Environment roomSizeHeight roomSizeWidth _) = zip rectangleCoordinates coordinates
  where (stepSizeHeight, stepSizeWidth) = (roomSizeHeight / fromIntegral parts, roomSizeWidth / fromIntegral parts)
        (partsHeight, partsWidth) =  ([0, stepSizeHeight..roomSizeHeight - stepSizeHeight], [0, stepSizeWidth..roomSizeWidth - stepSizeWidth])
        coordinates = concatMap (\x -> map (Coordinate x) partsHeight) partsWidth
        indexes = [0..parts - 1]
        rectangleCoordinates = concatMap (\x -> map (\y -> (x, y)) indexes) indexes

-- | Given a list of left down corner rectangles, build the whole data type
buildRectangles :: (Coordinate -> DetectionProbability) -> [(RectangleCoordinate, Coordinate)] -> M.Map RectangleCoordinate Rectangle
buildRectangles lowestProbability = M.fromList . map (fmap buildRectangle)
  where buildRectangle c = Rectangle c (lowestProbability c)

-- | Find the lowest probability possible for a rectangle
findLowestProbability :: Float -> Float -> [Detector] -> Coordinate -> DetectionProbability
findLowestProbability rectangleWidth rectangleSizeHeight detectors c
 | null inside = maximum $ map (\d -> computeDetectionProbability d c) detectors
 | otherwise   = DetectionProbability 1
  where (inside, outside) = partition (isInRectangle c . coordinate) detectors
        isInRectangle (Coordinate ax ay) (Coordinate bx by) =
          ax <= bx && (ax + rectangleWidth) >= bx && ay <= by && (ay + rectangleSizeHeight) >= by

-- | Convert the map to a graph, given that each element is linked to his
-- neigours and the edge weight is the element's probability
buildGraph :: M.Map RectangleCoordinate Rectangle -> Graph RectangleCoordinate DetectionProbability
buildGraph rectanglesMap = Graph $ M.fromList $ map buildVertice $ M.keys rectanglesMap
  where buildVertice o@(x, y) =
          (o, catMaybes [ (\(Rectangle _ d) -> EdgeTo coords d) <$> (rectanglesMap M.!? coords)
                        | (x', y') <- [(1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, -1)]
                        , coords <- [(x + x', y + y')]
                        ])
