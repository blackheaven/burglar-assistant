{-# LANGUAGE FlexibleContexts #-}

module IO(
           parseMapFile
         , printRounded
         ) where

import Text.Parsec
import Text.Printf

import Types

printRounded :: DetectionProbability -> String
printRounded (DetectionProbability x) = if last trimmed == '.' then [head trimmed] else trimmed
  where trimmed = reverse $ dropWhile (== '0') $ reverse $ printf "%0.3f" x

parseMapFile :: String -> Maybe Environment
parseMapFile content = case parse environmentParser "" content of
                         Left _  -> Nothing
                         Right e -> Just e

environmentParser :: Stream s m Char => ParsecT s u m Environment
environmentParser = do
  roomSize <- floatParser
  if roomSize == 0.0
    then fail "roomSize can not be zero"
    else return ()
  eol
  detectorsNumber <- integerParser
  eol
  detectors <- count detectorsNumber $ detectorParser roomSize
  eof
  return $ Environment roomSize detectors

detectorParser :: Stream s m Char => Float -> ParsecT s u m Detector
detectorParser roomSize = do
  coordinate <- coordinateParser roomSize
  option () eol
  return $ Detector coordinate

coordinateParser :: Stream s m Char => Float -> ParsecT s u m Coordinate
coordinateParser roomSize = do
  x <- floatParser
  space
  y <- floatParser
  if all (< roomSize) [x, y]
    then return $ Coordinate x y
    else fail "Coordinates beyond room size"

numberParser :: Stream s m Char => ParsecT s u m String
numberParser = many1 digit

integerParser :: Stream s m Char => ParsecT s u m Int
integerParser = read <$> numberParser

floatParser :: Stream s m Char => ParsecT s u m Float
floatParser = fmap read $ (++) <$> numberParser <*> decimal
    where decimal = option "" $ (:) <$> char '.' <*> numberParser

eol :: Stream s m Char => ParsecT s u m ()
eol = char '\n' >> return ()
