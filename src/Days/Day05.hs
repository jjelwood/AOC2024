{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Days.Day05 (runDay) where

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
import Debug.Trace (traceShowId)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    pairs <- pair `sepBy` endOfLine
    endOfLine
    endOfLine
    lists <- (decimal `sepBy` char ',') `sepBy` endOfLine
    return (pairs, lists)
    where
        pair = do
            a <- decimal
            char '|'
            b <- decimal
            return (a, b)

------------ TYPES ------------
type Input = ([(Int, Int)], [[Int]])

type OutputA = Int

type OutputB = Int

------------ PART A ------------
createOrdering :: [(Int, Int)] -> Map (Int, Int) Ordering
createOrdering pairs = Map.union (Map.fromList (map (\x -> (x, GT)) pairs)) (Map.fromList (map (\(a, b) -> ((b, a), LT)) pairs))

isOrdered :: Map (Int, Int) Ordering -> [Int] -> Bool -- Intrestingly this works? You only need to consider adjacent elements
isOrdered ordering list = all (\(a, b) -> (ordering Map.! (a, b)) == GT) $ zip list (tail list)

getMiddle :: [a] -> a
getMiddle xs = xs !! (length xs `div` 2)

partA :: Input -> OutputA
partA (pairs, lists) = sum $ map getMiddle $ filter (isOrdered ordering) lists
    where
        ordering = createOrdering pairs

------------ PART B ------------
partB :: Input -> OutputB
partB (pairs, lists) = sum $ map (getMiddle . sortBy (curry (ordering Map.!))) $ filter (not . isOrdered ordering) lists
    where
        ordering = createOrdering pairs
