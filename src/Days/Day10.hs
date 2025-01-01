module Days.Day10 (runDay) where

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
import Data.Attoparsec.Text
import Data.Void
import Data.Char (digitToInt)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = U.mapFromNestedLists <$> (many1 (digitToInt <$> digit) `sepBy` endOfLine)

------------ TYPES ------------
type Input = Map (Int, Int) Int

type OutputA = Int

type OutputB = Int

------------ PART A ------------
neighboursOf :: (Int, Int) -> [(Int, Int)]
neighboursOf (x, y) = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0), dy == 0 || dx == 0]

findNines :: Map (Int, Int) Int -> Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
findNines m s (x, y) | value == 9 = Set.singleton (x, y)
                     | otherwise = Set.unions $ map (\n -> findNines m (n `Set.insert` s) n) nexts
    where
        value = Map.findWithDefault 0 (x, y) m
        nexts = filter isTraversable $ neighboursOf (x, y)
        isTraversable neighbour =
            Set.notMember neighbour s &&
            Map.findWithDefault 0 neighbour m == value + 1

ratingA :: Map (Int, Int) Int -> (Int, Int) -> Int
ratingA m p = Set.size $ findNines m (Set.singleton p) p

partA :: Input -> OutputA
partA inp = sum $ map (ratingA inp) $ Map.keys $ Map.filter (== 0) inp

------------ PART B ------------
countPaths :: Map (Int, Int) Int -> Set (Int, Int) -> (Int, Int) ->  Int
countPaths m s (x, y) | value == 9 = 1
                      | otherwise = sum $ map (\n -> countPaths m (n `Set.insert` s) n) nexts
    where
        value = Map.findWithDefault 0 (x, y) m
        nexts = filter isTraversable $ neighboursOf (x, y)
        isTraversable neighbour =
            Set.notMember neighbour s &&
            Map.findWithDefault 0 neighbour m == value + 1

ratingB :: Map (Int, Int) Int -> (Int, Int) -> Int
ratingB m p = countPaths m (Set.singleton p) p

partB :: Input -> OutputB
partB inp = sum $ map (ratingB inp) $ Map.keys $ Map.filter (== 0) inp
