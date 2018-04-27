{-# LANGUAGE GADTs           #-}

module Smarties.TreeState (
    Scopeable(..),
    TreeState(..),
    _stack,
    _perception,
    makeTreeState
) where

import Smarties.TreeStack

import           Control.Lens


-- | Scopeable type class for use as computation state in behavior tree traversals
-- default implementation noops on all stack operations
class Scopeable p where
    stackSize :: p -> Int
    stackPush :: p -> p
    stackPop :: p -> p

-- | split of indexable and markable
-- note if both Stackable and Loopable, the looping index is expected to live on the stack
class Loopable p where
    loopIndex :: p -> Int
    incrementStackLoopIndex :: p -> p
    resetStackLoopIndex :: p -> p

class Markable p where 
    mark :: String -> p -> p

data TreeState x y where
    TreeState :: (TreeStackInfo x) => TreeStack x -> y -> TreeState x y

_stack :: Lens' (TreeState x y) (TreeStack x)
_stack f (TreeState x y) = fmap (\x' -> TreeState x' y) (f x)

_stackTopValue :: Lens' (TreeState x y) x
_stackTopValue = _stack . _top . _1

-- this warns because of lens nonsense
_stackTopLoopingIndex :: Getter (TreeState x y) Int
_stackTopLoopingIndex = _stack . _top . _2

_perception :: Lens' (TreeState x y) y
_perception f (TreeState x y) = fmap (\y' -> TreeState x y') (f y)

makeTreeState :: (TreeStackInfo x) => y -> TreeState x y
makeTreeState = TreeState (TreeStack [])

instance Scopeable (TreeState x y) where
    stackSize x = size $ view _stack x
    stackPush x = over _stack push x
    stackPop x = over _stack pop x

instance Loopable (TreeState x y) where
    loopIndex x = view _stackTopLoopingIndex x
    incrementStackLoopIndex x = (over (_stack . _top . _2)) (+1) x
    resetStackLoopIndex x = set (_stack . _top . _2) 0 x









