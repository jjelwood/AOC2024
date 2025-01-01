module Days.Day11 (runDay) where

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
import Debug.Trace (traceShowId, traceShow)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` char ' '

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . (!! 25) . iterate blinkMap . Map.fromList . map (, 1)

------------ PART B ------------
blinkMap :: Map Int Int -> Map Int Int
blinkMap m = Map.fromListWith (+) $ concatMap blinkMap' $ Map.toList m

blinkMap' :: (Int, Int) -> [(Int, Int)]
blinkMap' (0, c) = [(1, c)]
blinkMap' (x, c) | even ds = [(x `div` (10 ^ (ds `div` 2)), c), (x `mod` (10 ^ (ds `div` 2)), c)]
                 | otherwise = [(2024 * x, c)]
    where
        ds = ceiling $ logBase 10 (fromIntegral x + 1)

blink :: [Int] -> [Int]
blink [] = []
blink (0:xs) = 1 : blink xs
blink (x:xs) | even ds = x `div` (10 ^ (ds `div` 2)) : x `mod` (10 ^ (ds `div` 2)) : blink xs
             | otherwise = 2024 * x : blink xs
    where
        ds = ceiling $ logBase 10 (fromIntegral x + 1)

partB :: Input -> OutputB
partB = sum . (!! 75) . iterate blinkMap . Map.fromList . map (, 1)
