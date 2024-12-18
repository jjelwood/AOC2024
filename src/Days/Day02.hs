module Days.Day02 (runDay) where

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
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = level `sepBy` endOfLine
    where
        level = decimal `sepBy` char ' '

------------ TYPES ------------
type Input = [Report]
type Report = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
isSafeDifferences :: Report -> Bool
isSafeDifferences [] = True
isSafeDifferences [x] = True
isSafeDifferences (x:y:xs) = (d >= 1 && d <= 3) && isSafe (y:xs)
    where
        d = abs (x - y)

isIncreasing :: Report -> Bool
isIncreasing [] = True
isIncreasing [x] = True
isIncreasing (x:y:xs) = x <= y && isIncreasing (y:xs)

isDecreasing :: Report -> Bool
isDecreasing [] = True
isDecreasing [x] = True
isDecreasing (x:y:xs) = x >= y && isDecreasing (y:xs)

isSafe :: Report -> Bool
isSafe l = isSafeDifferences l && (isIncreasing l || isDecreasing l)

partA :: Input -> OutputA
partA = length . filter isSafe

------------ PART B ------------
remove :: [a] -> [[a]]
remove [] = []
remove (x:xs) = xs : map (x:) (remove xs)

partB :: Input -> OutputB
partB = length . filter (any isSafe . remove)
