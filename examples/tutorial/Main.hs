{-# LANGUAGE TypeSynonymInstances           #-}

module Main where

import Smarties
import System.Random
import System.Console.Haskeline
import System.Exit
import Control.Concurrent
import Control.Monad hiding (sequence)
import Control.Applicative ((<$>))
import Control.Monad.ST
import Control.Lens (over, _2)
import Prelude hiding (sequence)
import Data.List
import Data.List.Index (ifoldl)
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Text.Printf

type Pos = (Int, Int)

data Feelings = Happy | Sad | Hungry | Apathy deriving (Show)
data Doings = BlowingBubbles | Eating | Bored deriving (Show)

data Slime = Slime {
    _pos :: Pos,
    _feeling :: Feelings,
    _doings :: Doings,
    _weight :: Int
} deriving (Show)

renderSlime :: Slime -> String
renderSlime (Slime _ f _ _) = case f of
    Happy -> "😊"
    Sad -> "😟"
    Hungry -> "😋"
    Apathy -> "😐"

width :: Int
width = 20
height :: Int
height = 20
numberCells :: Int
numberCells = width * height
type Slimes = [Slime]
type TreeStateType = (Slimes, Slime)
type ActionType = (Slime -> Slimes)

-- |
-- our slime action is a special case of behavior tree action type where there should only ever be one action in the output of the tree
-- we do a runtime check here to make sure this is the case
-- unfortunately smarties currently does not support type level checking of this constraint :(.
extractHead :: [ActionType] -> ActionType
extractHead fs
    | null fs = (: [])
    | length fs == 1 = head fs
    | otherwise = error "slime behavior tree must only have one output"

-- |
-- puts slimes in a grid
makeSlimeGrid :: Slimes -> [Maybe Slime]
makeSlimeGrid slimes = runST $ do
    grid <- MV.replicate numberCells Nothing
    forM_ slimes $ \s@(Slime (x,y) _ _ _) -> MV.write grid (y*width+x) (Just s)
    V.toList <$> V.freeze grid

-- |
-- helper for writing slimes to console :)
renderSlimes :: Slimes -> String
renderSlimes = ifoldl func "" . makeSlimeGrid where
    func acc i x = output where
        nl = if i+1 `mod` width == 0 then "\n" else ""
        se = case x of
            Just s -> renderSlime s
            Nothing -> " "
        output = printf "%s %s%c" acc se nl

-- |
-- fuse slimes that share the same cell
fuseSlimes :: Slimes -> Slimes
fuseSlimes slimes =  runST $ do
    grid <- MV.replicate numberCells Nothing
    forM_ slimes $ \(Slime (x,y) _ _ w) ->
        MV.modify grid
            (\case
                Just (Slime _ _ _ w2) -> Just $ Slime (x,y) Happy Eating (w+w2)
                Nothing -> Nothing)
            (y*width+x)
    catMaybes . V.toList <$> V.freeze grid

slimeTree :: (RandomGen g) => NodeSequence g TreeStateType ActionType Int
slimeTree =
    -- weighted selector chooses the child with largest weight
    -- which is the (Float) monadic return value of the child NodeSequence
    utilityWeightedSelector []




-- |
-- run slimeTree for each slime collecting results
slimeCycle :: (RandomGen g) => g -> Slimes -> (g, Slimes)
slimeCycle g0 slimes = over _2 concat $ (mapAccumL runSlimeTree g0 slimes) where
    -- function to run slime tree over all slimes accumulating the RNG
    runSlimeTree g slime = (g', extractHead os slime) where
        (g', _, _, os) = execNodeSequence slimeTree g (slimes, slime)

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
        cycleOnce (g,s) = do
            let (g',s') = slimeCycle g s
            threadDelay 10000
            cycleOnce (g',s')
    cycleOnce (stdgen, genesis)
