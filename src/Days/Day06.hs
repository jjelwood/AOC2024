module Days.Day06 (runDay) where

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
import Data.Attoparsec.Text hiding (takeWhile, D)
import Data.Void
import Control.Applicative (asum)
import Debug.Trace (traceShowId, traceShow)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = findGuard . U.mapFromNestedLists . transpose <$> (many1 c `sepBy` endOfLine)
    where
        c = asum [
            char '.' >> return (Right False),
            char '#' >> return (Right True),
            char '^' >> return (Left U),
            char 'v' >> return (Left D),
            char '>' >> return (Left R),
            char '<' >> return (Left L)
            ]

findGuard :: Map (Int, Int) (Either Direction Bool) -> (Map (Int, Int) Bool, Guard)
findGuard m = (Map.map (\case Prelude.Left _ -> False; Prelude.Right b -> b) m, ((x, y), d))
    where
        ((x, y), d) = head $ Map.toList $ Map.mapMaybe (\case Right _ -> Nothing; Left d -> Just d) m

------------ TYPES ------------
type Input = (Map (Int, Int) Bool, Guard)
type Guard = ((Int, Int), Direction)
data Direction = U | D | L | R deriving (Show, Eq)

instance Ord Direction where
    compare U U = EQ
    compare U _ = LT
    compare _ U = GT
    compare D D = EQ
    compare D _ = LT
    compare _ D = GT
    compare L L = EQ
    compare L _ = LT
    compare _ L = GT
    compare R R = EQ

type OutputA = Int

type OutputB = Int

------------ PART A ------------
offset :: Direction -> (Int, Int)
offset U = (0, -1)
offset D = (0, 1)
offset L = (-1, 0)
offset R = (1, 0)

turn :: Direction -> Direction
turn U = R
turn R = D
turn D = L
turn L = U

infront :: ((Int, Int), Direction) -> (Int, Int)
infront ((x, y), d) = (x + dx, y + dy)
    where
        (dx, dy) = offset d

move :: Map (Int, Int) Bool -> Guard -> Guard
move m g@(p@(x, y), d)
    | Map.findWithDefault False (infront g) m = (p, turn d) -- If there's something in front, turn
    | otherwise = (infront g, d) -- Otherwise move forward

states :: Input -> [(Guard, Set Guard)]
states (m, g@(x, y)) = takeWhile (isInBounds . fst . fst) $ iterate next (g, Set.singleton g)
    where
        next (g', v) = (g'', Set.insert g'' v)
            where
                g'' = move m g'
        isInBounds (x, y) = x >= xmin &&  x <= xmax && y >= ymin && y <= ymax
        (xmin, xmax, ymin, ymax) = U.mapBoundingBox m


partA :: Input -> OutputA
partA = Set.size . Set.map fst . snd . last . states

------------ PART B ------------
candidates :: Set Guard -> Set (Int, Int)
candidates = Set.map infront

causesLoop :: Input -> (Int, Int) -> Bool
causesLoop (m, g) (x, y)
    | (x < xmin) || (x > xmax) || (y < ymin) || (y > ymax) = False -- If we're at the edge, we can't place a new obstacle
    | otherwise = any (elem <$> ((,) <$> infront <*> snd) . fst <*> snd) $ states (m', g)
    where
        (xmin, xmax, ymin, ymax) = U.mapBoundingBox m
        m' = Map.insert (x, y) True m

partB :: Input -> OutputB
partB inp@(m, _) = length $ Set.filter (causesLoop inp) $ candidates $ snd $ last $ states inp
