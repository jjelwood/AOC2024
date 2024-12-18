module Days.Day07 (runDay) where

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
import qualified Data.Bifunctor
import Debug.Trace (traceShow)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = row `sepBy` endOfLine
    where
        row = do
            target <- decimal
            string ": "
            ns <- decimal `sepBy` char ' '
            return (target, ns)

------------ TYPES ------------
type Input = [(Int, [Int])]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
isCalibratableA :: (Int, [Int]) -> Bool
isCalibratableA (target, [n]) = target == n
isCalibratableA (target, n:ns) = isCalibratableA (target - n, ns) || (target `mod` n == 0 && isCalibratableA (target `div` n, ns))

partA :: Input -> OutputA
partA = sum . map fst . filter isCalibratableA . map (Data.Bifunctor.second reverse)

------------ PART B ------------
isCalibratableB :: (Int, [Int]) -> Bool
isCalibratableB (target, [n]) = target == n
isCalibratableB (target, n:ns) =
    isCalibratableB (target - n, ns) ||
    (target `mod` n == 0 && isCalibratableB (target `div` n, ns)) ||
    (target `mod` digitsn == n && isCalibratableB (target `div` digitsn, ns))
    where
        digitsn = 10 ^ length (show n)

partB :: Input -> OutputB
partB = sum . map fst . filter isCalibratableB . map (Data.Bifunctor.second reverse)
