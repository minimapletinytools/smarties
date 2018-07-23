{-# LANGUAGE TypeSynonymInstances           #-}

module Main where

import Smarties
import System.Random
import Control.Monad hiding (sequence)
import Control.Monad.ST
import Prelude hiding (sequence)
import Data.List.Index (ifoldl)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Text.Printf

type Pos = (Int, Int)

data Feelings = Happy | Sad | Hungry | Apathy deriving (Show)
data Doings = BlowingBubbles | Eating | Bored deriving (Show)

data Slime = Slime {
    _pos :: Pos,
    _feeling :: Feelings,
    _doings :: Doings
} deriving (Show)

renderSlime :: Slime -> String
renderSlime (Slime _ f _) = case f of
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
type ActionType = (Slime -> Slime)

-- |
-- puts slimes in a grid
makeSlimeGrid :: Slimes -> [Maybe Slime]
makeSlimeGrid slimes = runST $ do
    grid <- MV.replicate numberCells Nothing
    forM_ slimes $ \s@(Slime (x,y) _ _) -> MV.write grid (y*width+x) (Just s)
    V.freeze grid >>= return . V.toList

renderSlimes :: Slimes -> String
renderSlimes = ifoldl func "" . makeSlimeGrid where
    func acc i x = output where
        nl = if i+1 `mod` width == 0 then "\n" else ""
        se = case x of
            Just s -> renderSlime s
            Nothing -> " "
        output = printf "%s %s%c" acc se nl

main :: IO ()
main = undefined
