module Days.Day09 (runDay) where

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
import Control.Applicative ((<|>))
import Data.Char (digitToInt)
import Data.Either (isRight, isLeft, fromRight)
import Debug.Trace (traceShowId, trace)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Vec.fromList . concatMap (\(i, d) -> if even i then replicate d (Right (i `div` 2)) else replicate d (Left '.')) . zip [0..] <$>
    many1 (digitToInt <$> digit)

------------ TYPES ------------
type Input = Vector (Either Char Int)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
moveFiles :: Vector (Either Char Int) -> Vector Int
moveFiles v = Vec.map (fromRight (error "Unexpected Left value")) $ Vec.take (length files) $
    v Vec.// [(i_blank, file) | ((i_file, file), (i_blank, Left _)) <- zip files blanks, i_file > i_blank]
    where
        files = reverse $ filter (isRight . snd) $ Vec.toList $ Vec.indexed v
        blanks = filter (isLeft . snd) $ Vec.toList $ Vec.indexed v

partA :: Input -> OutputA
partA = sum . Vec.map ((*) <$> fst <*> snd) . Vec.indexed . moveFiles

------------ PART B ------------
moveFileChunks :: Vector (Either Char Int) -> Vector (Either Char Int)
moveFileChunks v = Vec.map snd $ Vec.concat $ foldl' moveFileChunk groups files
    where
        groups = Vec.groupBy (\a b -> snd a == snd b) $ Vec.indexed v
        files = reverse $ filter (isRight . snd . Vec.head) groups

startIndex :: Vector (Int, Either Char Int) -> Int
startIndex = fst . Vec.head

isBlankGroup :: Vector (Int, Either Char Int) -> Bool
isBlankGroup = isLeft . snd . Vec.head

moveFileChunk :: [Vector (Int, Either Char Int)] -> Vector (Int, Either Char Int) -> [Vector (Int, Either Char Int)]
moveFileChunk [] _ = []
moveFileChunk (group:groups) fileGroup
    -- gone past the fileGroup, so don't change anything
    | startIndex group > startIndex fileGroup = group : groups
    -- insert the fileGroup into the blank group and split them
    | isBlankGroup group && Vec.length group >= Vec.length fileGroup =  
        if null newGroup then newFileGroup' : removeFileGroup groups else newFileGroup' : newGroup : removeFileGroup groups
    -- try the next group
    | otherwise = group : moveFileChunk groups fileGroup
    where
        removeFileGroup = map (\g -> if g == fileGroup then Vec.map (\(i, v) -> (i, Left '.')) g else g)
        groupn = snd $ Vec.head fileGroup
        (newFileGroup, newGroup) = Vec.splitAt (Vec.length fileGroup) group
        newFileGroup' = Vec.map (\(i, v) -> (i, groupn)) newFileGroup


partB :: Input -> OutputB
partB = sum . Vec.map ((*) <$> fst <*> fromRight 0 . snd) . Vec.indexed . moveFileChunks
-- partB = undefined
