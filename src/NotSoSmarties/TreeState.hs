{-# LANGUAGE GADTs           #-}

module NotSoSmarties.TreeState (
    TreeState(..),
    BasicTreeState(..),
    _bState,
    AdvancedTreeState(..),
    _stack,
    _perception,
    _generator,
    makeAdvancedTreeState
) where

import NotSoSmarties.TreeStack

import           Control.Lens
import           System.Random (RandomGen (..))


-- | TreeState type class for use as computation state in behavior tree traversals
-- default implementation noops on all stack operations
class (RandomGen p) => TreeState p where
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

instance RandomGen () where
    next () = (undefined,())
    split () = ((),())
    
instance TreeState ()

data BasicTreeState p g where
    BasicTreeState :: (RandomGen g) => p -> g -> BasicTreeState p g

_bState :: Lens' (BasicTreeState p g) p
_bState f (BasicTreeState x y) = fmap (\x' -> BasicTreeState x' y) (f x)

instance (RandomGen g) => RandomGen (BasicTreeState p g) where
    -- switch to using lenses :D
    next (BasicTreeState p g) = (fst $ next g, BasicTreeState p (snd $ next g))
    split (BasicTreeState p g) = (over each) (BasicTreeState p) (split g)

-- | all default implementation is good
instance (RandomGen g) => TreeState (BasicTreeState p g)

data AdvancedTreeState x y g where
    AdvancedTreeState :: (TreeStackInfo x, RandomGen g) => TreeStack x -> y -> g -> AdvancedTreeState x y g

_stack :: Lens' (AdvancedTreeState x y g) (TreeStack x)
_stack f (AdvancedTreeState x y g) = fmap (\x' -> AdvancedTreeState x' y g) (f x)

_stackTopValue :: Lens' (AdvancedTreeState x y g) x
_stackTopValue = _stack . _top . _1

-- this warns because of lens nonsense
_stackTopLoopingIndex :: Getter (AdvancedTreeState x y g) Int
_stackTopLoopingIndex = _stack . _top . _2

_perception :: Lens' (AdvancedTreeState x y g) y
_perception f (AdvancedTreeState x y g) = fmap (\y' -> AdvancedTreeState x y' g) (f y)

_generator :: Lens' (AdvancedTreeState x y g) g
_generator f (AdvancedTreeState x y g) = fmap (\g' -> AdvancedTreeState x y g') (f g)

makeAdvancedTreeState :: (TreeStackInfo x, RandomGen g) => y -> g -> AdvancedTreeState x y g
makeAdvancedTreeState = AdvancedTreeState (TreeStack [])

instance (RandomGen g) => RandomGen (AdvancedTreeState x y g) where
    next (AdvancedTreeState x y g) = (fst $ next g, AdvancedTreeState x y (snd $ next g))
    split (AdvancedTreeState x y g) = (over each) (AdvancedTreeState x y) (split g)

instance (RandomGen g) => TreeState (AdvancedTreeState x y g) where
    stackSize x = size $ view _stack x
    stackPush x = over _stack push x
    stackPop x = over _stack pop x
    stackLoopIndex x = view _stackTopLoopingIndex x
    incrementStackLoopIndex x = (over (_stack . _top . _2)) (+1) x
    resetStackLoopIndex x = set (_stack . _top . _2) 0 x









