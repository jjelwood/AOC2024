module Days.Day08 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text hiding (takeWhile)
import Data.Void
import GHC.Exts (groupWith)
import Control.Applicative ((<|>))
import Debug.Trace (traceShowId)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = ((,) <$> Map.filter (/= '.') <*> U.mapBoundingBox) . U.mapFromNestedLists <$> (many1 (char '.' <|> letter <|> digit) `sepBy` endOfLine)

------------ TYPES ------------
type Input = (Map (Int, Int) Char, (Int, Int, Int, Int))

type OutputA = Int

type OutputB = Int

------------ PART A ------------
getPairGroups :: Map (Int, Int) Char -> [[((Int, Int), (Int, Int))]]
getPairGroups m = [ [ (a, b) | a <- group, b <- group, a < b ] | group <- groups]
    where
        groups = map (map fst) $ groupWith snd $ Map.toList m

findAnodesA :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
findAnodesA ((x1, y1), (x2, y2)) = [(x1 - dx, y1 - dy), (x2 + dx, y2 + dy)]
    where
        dy = y2 - y1
        dx = x2 - x1

isInGrid :: (Int, Int, Int, Int) -> (Int, Int) -> Bool
isInGrid (xmin, xmax, ymin, ymax) (x, y) = x >= xmin && x <= xmax && y >= ymin && y <= ymax

partA :: Input -> OutputA
partA (inp, bounds) = length $ nub $ concatMap (concatMap (filter (isInGrid bounds) . findAnodesA)) pairGroups
    where
        pairGroups = getPairGroups inp

------------ PART B ------------
findAnodesB :: (Int, Int, Int, Int) -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
findAnodesB bounds ((x1, y1), (x2, y2)) = takeWhile (isInGrid bounds) backwardsAnodes ++ takeWhile (isInGrid bounds) forwardsAnodes
    where
        dy = y2 - y1
        dx = x2 - x1
        backwardsAnodes = iterate (\(x, y) -> (x - dx, y - dy)) (x1, y1)
        forwardsAnodes = iterate (\(x, y) -> (x + dx, y + dy)) (x2, y2)

partB :: Input -> OutputB
partB (inp, bounds) = length $ nub $ concatMap (concatMap (findAnodesB bounds)) pairGroups
    where
        pairGroups = getPairGroups inp
