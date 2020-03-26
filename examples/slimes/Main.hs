{-|
Module      : Main
Description : Slime Game
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@gmail.com
Stability   : experimental

Example simulating colony of slimes with feelings using smarties.
Unfinished and unworking. You're welcome to fix it for me :).
-}


{-# LANGUAGE TypeSynonymInstances           #-}

module Main where

import Smarties
import System.Random
import Control.Concurrent
import Control.Monad hiding (sequence)
import Control.Applicative ((<$>))
import Control.Monad.ST
import Lens.Micro
import Lens.Micro.TH
import Prelude hiding (sequence)
import Data.List
import Data.List.Index (ifoldl)
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Text.Printf

type Pos = (Int, Int)

addPos :: Pos -> Pos -> Pos
addPos (x1,y1) (x2,y2) = (x1+x2, y1+y2)

data Feelings = Happy | Sad | Hungry | Apathy deriving (Show, Eq)
data Doings = BlowingBubbles | Eating | Bored deriving (Show)

data Slime = Slime {
  _pos :: Pos,
  _feeling :: Feelings,
  _doings :: Doings,
  _weight :: Int
} deriving (Show)

makeLenses ''Slime

-- world size parameters
width :: Int
width = 20
height :: Int
height = 20
numberCells :: Int
numberCells = width * height
neighbors :: [Pos]
neighbors = [(-1,0),(1,0),(0,-1),(0,1)]


wrapCoords :: Pos -> Pos
wrapCoords (x,y) = (x `mod` width, y `mod` height)

-- TODO make sure this does what you want with neg coordinates...
wrapFlattenCoords :: Pos -> Int
wrapFlattenCoords (x,y) = (y `mod` height) * width + x `mod` width

-- behavior tree types
type Slimes = [Slime]
type SlimeGrid = V.Vector (Maybe Slime)
type PerceptionType = (SlimeGrid, Slime)

-- slime action is a little weird, for example if the output is [\x->[x],\x->[x]] this will actually make 2 copies of the slime in the same spot.
type ActionType = (Slime -> Slimes)

-- | extract slime that is being operated on from behavior tree perception
getMyself :: NodeSequence g PerceptionType ActionType Slime
getMyself = do
  (_, s) <- getPerception
  return s

-- | extract a neighboring slime
getSlimeRel :: Pos -> NodeSequence g PerceptionType ActionType (Maybe Slime)
getSlimeRel p = do
  (grid, _) <- getPerception
  return $ grid V.! wrapFlattenCoords p

-- | get a list of neigboring slimes
getNeighborSlimes :: NodeSequence g PerceptionType ActionType Slimes
getNeighborSlimes = do
  (grid, s) <- getPerception
  return . mapMaybe ((grid V.!) . wrapFlattenCoords . addPos (_pos s)) $ neighbors

-- behavior tree nodes
-- DELETE
conditionSlimeIsFeeling :: Feelings -> Slime -> NodeSequence g PerceptionType ActionType ()
conditionSlimeIsFeeling f s = fromCondition $
  SimpleCondition (\_ -> _feeling s == f)

actionMoveSlime :: Pos -> NodeSequence g PerceptionType ActionType ()
actionMoveSlime p = fromAction $
  SimpleAction (\_ -> \s -> [set pos (wrapCoords p) s])

-- |
actionMoveSlimeRel :: Pos -> NodeSequence g PerceptionType ActionType ()
actionMoveSlimeRel p = fromAction $
  SimpleAction (\_ -> \s -> [over pos (wrapCoords . addPos p) s])

actionSlime :: NodeSequence g PerceptionType ActionType ()
actionSlime = actionMoveSlimeRel (0,0)

actionCloneSlimeRel :: Pos -> NodeSequence g PerceptionType ActionType ()
actionCloneSlimeRel p = do
  actionMoveSlimeRel p
  actionSlime


-- | for testing
potatoTree :: NodeSequence g PerceptionType ActionType ()
potatoTree = do
  actionMoveSlimeRel (1,-1)

-- | our behavior tree
slimeTree :: NodeSequence g PerceptionType ActionType ()
slimeTree = do
  nbs <- getNeighborSlimes
  s <- getMyself
  selector [
    -- no neighbors
    do
      condition (null nbs)
      case _feeling s of
        Happy -> actionCloneSlimeRel (1,0)
        Sad -> actionCloneSlimeRel (-1,0)
        Hungry -> actionMoveSlimeRel (0,1)
        _ -> actionSlime
    -- 1 neighbor
    , do
      condition (length nbs == 1)
      let
        nb = head nbs
      case (_feeling nb, _feeling s) of
        (Happy, Happy) -> actionMoveSlimeRel (0,1)
        (Sad, Sad) -> actionCloneSlimeRel (0,-1)
        (Sad, Happy) -> actionMoveSlime (_pos nb)
        _ -> actionSlime
    -- >1 neighbors
    , actionMoveSlimeRel (0,1)
    ]

    -- > 1 neighbor case
    -- don't do anything, this means the slime will die :(


-- | DELETE
-- our slime action is a special case of behavior tree action type where there should only ever be one action in the output of the tree
-- we do a runtime check here to make sure this is the case
-- unfortunately smarties currently does not support type level checking of this constraint :(.
extractHead :: [ActionType] -> ActionType
extractHead fs
  | null fs = (: [])
  | length fs == 1 = head fs
  | otherwise = error "slime behavior tree must only have one output"

-- | puts slimes in a grid
makeSlimeGrid :: Slimes -> SlimeGrid
makeSlimeGrid slimes = runST $ do
  grid <- MV.replicate numberCells Nothing
  forM_ slimes $ \s@(Slime (x,y) _ _ _) -> MV.write grid (y*width+x) (Just s)
  V.freeze grid

-- | helper for writing slimes to console :)
renderSlime :: Slime -> String
renderSlime (Slime _ f _ _) = case f of
  Happy -> "😊"
  Sad -> "😟"
  Hungry -> "😋"
  Apathy -> "😐"

-- | helper for writing slimes to console :)
renderSlimes :: Slimes -> String
renderSlimes = Data.List.Index.ifoldl func "" . V.toList . makeSlimeGrid where
  func acc i x = output where
    nl = if (i+1) `mod` width == 0 then "\n" else ""
    se = case x of
      Just s -> renderSlime s
      Nothing -> "🌱"
    output = printf "%s%s%s" acc se nl

-- | fuse slimes that share the same cell
fuseSlimes :: Slimes -> Slimes
fuseSlimes slimes =  runST $ do
  grid <- MV.replicate numberCells Nothing
  forM_ slimes $ \s@(Slime (x,y) _ _ w) ->
    MV.modify grid
      (\case
        -- fused slimes just ate each other so they are
        Just (Slime _ _ _ w2) -> Just $ Slime (x,y) Happy Eating (w+w2)
        Nothing -> Just s)
      (y*width+x)
  catMaybes . V.toList <$> V.freeze grid

-- | run slimeTree for each slime collecting results
slimeCycle :: g -> Slimes -> (g, Slimes)
slimeCycle g0 slimes = over _2 (fuseSlimes . concat) (mapAccumL runSlimeTree g0 slimes) where
  -- function to run slime tree over all slimes accumulating the RNG
  runSlimeTree g slime = (g', concat (map ($ slime) os)) where
    (g', _, _, os) = execNodeSequence slimeTree g (makeSlimeGrid slimes, slime)

applyNtimes :: (Num n, Ord n) => n -> (a -> a) -> a -> a
applyNtimes 1 f x = f x
applyNtimes n f x = f (applyNtimes (n-1) f x)


{-exitLoop :: IO ()
exitLoop = do
  minput <- getInputChar "% "
  case minput of
    Nothing -> return ()
    Just 'q' -> exitSuccess-}

main :: IO ()
main = do
  --forkIO exitLoop
  stdgen <- getStdGen
  let
    genesis = [Slime (0,0) Sad Bored 1] -- 😢
    --outSlimes = applyNtimes 100 (\(g,s) -> slimeCycle g s) (stdgen, genesis)
    cycleOnce (n :: Int) (g,s) = do
      putStrLn "done"
      putStrLn $ renderSlimes s
      let (g',s') = slimeCycle g s
      putStrLn $ "gen " ++ show n
      threadDelay 100000
      cycleOnce (n+1) (g',s')
  cycleOnce 0 (stdgen, genesis)
