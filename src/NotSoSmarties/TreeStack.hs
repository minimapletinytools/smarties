{-# LANGUAGE GADTs           #-}

module NotSoSmarties.TreeStack (
    TreeStackInfo(..),
    TreeStack(..),
    size,
    push,
    pop,
    top,
    _top
) where

import           Control.Lens

class TreeStackInfo x where
    inherit :: x -> x
    inherit = id
    empty :: x

instance TreeStackInfo () where
    empty = ()



-- TODO add built in indexing for loops
-- | stack grows via cons, i.e. head of stack is top of stack
data TreeStack x where
    TreeStack :: (TreeStackInfo x) => [(x, Int)] -> TreeStack x

size :: TreeStack x -> Int
size (TreeStack xs) = length xs

push :: TreeStack x -> TreeStack x
push (TreeStack xs) = TreeStack $ (newx, newi):xs where
    newx = inherit . fst . head $ xs
    newi = snd . head $ xs

pop :: TreeStack x -> TreeStack x
pop (TreeStack xs) = case xs of
    _:hs -> TreeStack hs
    []   -> error "can not pop an empty stack"

top :: TreeStack x -> (x, Int)
top (TreeStack xs) = case xs of
    h:_ -> h
    [] -> error "can not top an empty stack"

_top :: Lens' (TreeStack x) (x, Int)
_top f (TreeStack (x:xs)) = fmap (\x' -> TreeStack (x':xs)) (f x)
_top _ (TreeStack []) = error "can not top an empty stack"

{-resetLoopingIndex :: TreeStack x -> TreeStack x
resetLoopingIndex (TreeStack xs) = case xs of
    h:hs -> TreeStack $ (fst h, 0):hs
    []   -> error "can not reset looping index on an empty stack"
    -}
