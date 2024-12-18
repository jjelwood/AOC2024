module Days.Day01 (runDay) where

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
inputParser = ((,) <$> decimal <* string "   " <*> decimal) `sepBy` endOfLine

------------ TYPES ------------
type Input = [(Int, Int)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
makeLists :: [(Int, Int)] -> ([Int], [Int])
makeLists = foldr (\(a, b) (l1, l2) -> (a : l1, b : l2)) ([], [])

partA :: Input -> OutputA
partA inp = sum $ zipWith (\a b -> abs (a - b)) (sort l1) (sort l2)
    where
        (l1,  l2) = makeLists inp

------------ PART B ------------
partB :: Input -> OutputB
partB inp = sum $ map (\i1 -> length (filter (== i1) l2) * i1) l1
    where
        (l1,  l2) = makeLists inp
