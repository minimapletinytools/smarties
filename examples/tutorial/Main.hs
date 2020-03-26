{-|
Module      : Main
Description : Conway Game of Life example
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@gmail.com
Stability   : experimental
-}

module Main where

import           Control.Concurrent
import           Control.Monad      hiding (sequence)
import           Data.List
import           Data.List.Index    (ifoldl)
import qualified Data.Vector        as V
import           Prelude            hiding (sequence)
import           Smarties
import           System.Random
import           Text.Printf

-- world size parameters
width :: Int
width = 20
height :: Int
height = 20
numberCells :: Int
numberCells = width * height

-- types
type Pos = (Int, Int)

addPos :: Pos -> Pos -> Pos
addPos (x1,y1) (x2,y2) = (x1+x2, y1+y2)

wrapCoords :: Pos -> Pos
wrapCoords (x,y) = (x `mod` width, y `mod` height)

flattenCoords :: Pos -> Int
flattenCoords (x,y) = y * width + x

wrapFlattenCoords :: Pos -> Int
wrapFlattenCoords = flattenCoords . wrapCoords

indexToPos :: Int -> Pos
indexToPos p = (p `mod` width, p `div` width)

neighbors :: [Pos]
neighbors = [(-1,0),(1,0),(0,-1),(0,1),(1,1),(1,-1),(-1,1),(-1,-1)]

countTrue :: [Bool] -> Int
countTrue = foldl (\acc x -> if x then acc+1 else acc) 0

-- behavior tree types
type Grid = V.Vector Bool
type PerceptionType = (Pos, Grid)
type ActionType = Bool

-- behavior tree methods
countNeighbors  :: NodeSequence g PerceptionType ActionType Int
countNeighbors = do
  (pos, grid) <- getPerception
  return . countTrue . map ((grid V.!) . wrapFlattenCoords . addPos pos) $ neighbors

ifNeighborsMoreThan :: Int -> NodeSequence g PerceptionType ActionType ()
ifNeighborsMoreThan x = do
  n <- countNeighbors
  condition (n > x)

ifNeighborsLessThan :: Int -> NodeSequence g PerceptionType ActionType ()
ifNeighborsLessThan = flipResult . ifNeighborsMoreThan . (\x -> x-1)

die :: NodeSequence g PerceptionType ActionType ()
die = fromAction $ SimpleAction (\_ -> False)

born :: NodeSequence g PerceptionType ActionType ()
born = fromAction $ SimpleAction (\_ -> True)

ifAlive :: NodeSequence g PerceptionType ActionType ()
ifAlive = do
  (pos, grid) <- getPerception
  condition $ grid V.! (wrapFlattenCoords pos)

ifDead :: NodeSequence g PerceptionType ActionType ()
ifDead = flipResult $ ifAlive

-- | our behavior tree
-- note rule 2. is a just a noop (Any live cell with two or three live neighbors lives on to the next generation.)
conwayTree :: NodeSequence g PerceptionType ActionType ()
conwayTree = do
  selector [
    -- rule 1. Any live cell with fewer than two live neighbors dies, as if by underpopulation.
    do
      ifAlive
      ifNeighborsLessThan 2
      die
    -- rule 3. Any live cell with more than three live neighbors dies, as if by overpopulation.
    , do
      ifAlive
      ifNeighborsMoreThan 3
      die
    -- rule 4. Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.
    , do
      ifDead
      ifNeighborsMoreThan 2
      ifNeighborsLessThan 4
      born]

-- useful for testing :)
potatoTree :: NodeSequence g PerceptionType ActionType ()
potatoTree = do
  selector [
    do
      ifDead
      born
    , do
      ifAlive
      die]


-- | helper for writing grid to console :)
renderGrid :: Grid -> String
renderGrid = Data.List.Index.ifoldl func "" . V.toList where
    func acc i x = output where
        nl = if (i+1) `mod` width == 0 then "\n" else ""
        se = case x of
            True  -> "😱"
            False -> "🌱"
        output = printf "%s%s%s" acc se nl


-- | do conway rules for each grid cell
-- TODO make this run concurrently :). Need to pregen list of generators.
runConway :: g -> Grid -> (g, Grid)
runConway g0 grid = (g', V.zipWith ($) (V.fromList fns) grid) where
  (g', fns) = mapAccumL runTree g0 (map indexToPos [0..numberCells])
  runTree g pos = (g'', if null os then id else \_ -> last os) where
    (g'', _, _, os) = execNodeSequence conwayTree g (pos, grid)

-- | go!
main :: IO ()
main = do
    stdgen <- getStdGen
    let
        genesis = V.fromList . take numberCells $ randoms stdgen
        cycleOnce (n :: Int) (g, s) = do
            putStrLn "done"
            printf "gen %d\n" n
            putStrLn $ renderGrid s
            let (g',s') = runConway g s
            threadDelay 100000
            cycleOnce (n+1) (g',s')
    cycleOnce 0 (stdgen, genesis)
