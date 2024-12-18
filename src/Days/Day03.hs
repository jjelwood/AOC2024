module Days.Day03 (runDay) where

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
import Control.Applicative (Alternative(many, (<|>)))
import Data.Either (rights)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 (mul <|> do' <|> dont <|> (anyChar >> return Corrupted)) `sepBy` endOfLine
    where
        mul = do
            _ <- string "mul("
            x <- decimal
            _ <- string ","
            y <- decimal
            _ <- string ")"
            return $ Mul (x * y)
        dont = string "don't()" >> return Dont
        do' = string "do()" >> return Do

------------ TYPES ------------
data Op = Corrupted | Mul { value :: Int } | Do | Dont deriving (Show, Eq)
type Input = [[Op]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
isMul :: Op -> Bool
isMul (Mul _) = True
isMul _ = False

partA :: Input -> OutputA
partA = sum . map (sum . map value . filter isMul)

------------ PART B ------------
calculate :: [Op] -> Int
calculate = fst . foldl f (0, True)
    where
        f (n, True) (Mul x) = (n + x, True)
        f (n, False) (Mul x) = (n, False)
        f (n, _) Do = (n, True)
        f (n, _) Dont  = (n, False)
        f (n, d) Corrupted = (n, d)

partB :: Input -> OutputB
partB = sum . map calculate
