{-# LANGUAGE GADTs           #-}

module Smarties.TreeState (
    TreeState(..),
    BasicTreeState(..),
    _bState,
    AdvancedTreeState(..),
    _stack,
    _perception,
    makeAdvancedTreeState
) where

import Smarties.TreeStack

import           Control.Lens


-- | TreeState type class for use as computation state in behavior tree traversals
-- default implementation noops on all stack operations
class TreeState p where
    stackSize :: p -> Int
    stackSize _ = 0
    stackPush :: p -> p
    stackPush = id
    stackPop :: p -> p
    stackPop = id
    stackLoopIndex :: p -> Int
    stackLoopIndex _ = 0
    incrementStackLoopIndex :: p -> p
    incrementStackLoopIndex = id
    resetStackLoopIndex :: p -> p
    resetStackLoopIndex = id

instance TreeState ()

data BasicTreeState p where
    BasicTreeState :: p -> BasicTreeState p 

_bState :: Lens' (BasicTreeState p) p
_bState f (BasicTreeState x) = fmap (\x' -> BasicTreeState x') (f x)

-- | all default implementation is good
instance TreeState (BasicTreeState p)

data AdvancedTreeState x y where
    AdvancedTreeState :: (TreeStackInfo x) => TreeStack x -> y -> AdvancedTreeState x y

_stack :: Lens' (AdvancedTreeState x y) (TreeStack x)
_stack f (AdvancedTreeState x y) = fmap (\x' -> AdvancedTreeState x' y) (f x)

_stackTopValue :: Lens' (AdvancedTreeState x y) x
_stackTopValue = _stack . _top . _1

-- this warns because of lens nonsense
_stackTopLoopingIndex :: Getter (AdvancedTreeState x y) Int
_stackTopLoopingIndex = _stack . _top . _2

_perception :: Lens' (AdvancedTreeState x y) y
_perception f (AdvancedTreeState x y) = fmap (\y' -> AdvancedTreeState x y') (f y)

makeAdvancedTreeState :: (TreeStackInfo x) => y -> AdvancedTreeState x y
makeAdvancedTreeState = AdvancedTreeState (TreeStack [])

instance TreeState (AdvancedTreeState x y) where
    stackSize x = size $ view _stack x
    stackPush x = over _stack push x
    stackPop x = over _stack pop x
    stackLoopIndex x = view _stackTopLoopingIndex x
    incrementStackLoopIndex x = (over (_stack . _top . _2)) (+1) x
    resetStackLoopIndex x = set (_stack . _top . _2) 0 x









