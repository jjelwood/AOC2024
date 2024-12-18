module Days.Day04 (runDay) where

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
inputParser = Vec.fromList <$> (Vec.fromList <$> many1 letter) `sepBy` endOfLine

------------ TYPES ------------
type Input = Vector (Vector Char)
data Direction = N | NE | E | SE | S | SW | W | NW deriving (Show, Eq)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
offset :: Direction -> (Int, Int)
offset N = (-1, 0)
offset NE = (-1, 1)
offset E = (0, 1)
offset SE = (1, 1)
offset S = (1, 0)
offset SW = (1, -1)
offset W = (0, -1)
offset NW = (-1, -1)

lookupP :: Vector (Vector Char) -> (Int, Int) -> Char
lookupP grid (i, j) = grid Vec.! i Vec.! j

searchWord :: Vector (Vector Char) -> String -> (Int, Int) -> Int
searchWord grid word (i, j) = length $ filter (searchWordDir grid word (i, j)) [N, NE, E, SE, S, SW, W, NW]

searchWordDir :: Vector (Vector Char) -> String -> (Int, Int) -> Direction -> Bool
searchWordDir grid word (i, j) dir = word `isPrefixOf` searchWordDir' grid (i, j) dir

searchWordDir' :: Vector (Vector Char) -> (Int, Int) -> Direction -> String
searchWordDir' grid (i, j) dir 
    | i < 0 || j < 0 || i >= Vec.length grid || j >= Vec.length (grid Vec.! i) = ""
    | otherwise = lookupP grid (i, j) : searchWordDir' grid (i + di, j + dj) dir  
    where (di, dj) = offset dir    

indices :: Vector (Vector a) -> [(Int, Int)]
indices grid = [(i, j) | i <- [0..Vec.length grid - 1], j <- [0..Vec.length (grid Vec.! i) - 1]]

partA :: Input -> OutputA
partA inp = sum $ map (searchWord inp "XMAS") $ indices inp

------------ PART B ------------
isXMas :: Vector (Vector Char) -> (Int, Int) -> Bool
isXMas grid (i, j) =
    lookupP grid (i, j) == 'A' && 
    (
        (cAt NE 'M' && cAt NW 'M' && cAt SE 'S' && cAt SW 'S') ||
        (cAt NE 'S' && cAt NW 'S' && cAt SE 'M' && cAt SW 'M') ||
        (cAt NE 'M' && cAt NW 'S' && cAt SE 'M' && cAt SW 'S') ||
        (cAt NE 'S' && cAt NW 'M' && cAt SE 'S' && cAt SW 'M')
    )
    where
        cAt dir c | i' < 0 || j' < 0 || i' >= Vec.length grid || j' >= Vec.length (grid Vec.! i') = False
                  | otherwise = lookupP grid (i', j') == c
            where 
                (di, dj) = offset dir
                (i', j') = (i + di, j + dj)

partB :: Input -> OutputB
partB inp = length $ filter (isXMas inp) $ indices inp
