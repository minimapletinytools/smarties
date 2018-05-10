{-|
Module      : Builders
Description : Functions and types pertaining to DNA and Genes
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental
-}
module Smarties.Builders (
    -- $helper1link
    Utility(..),
    Perception(..),
    Action(..),
    Condition(..),
    SelfAction(..),
    fromUtility,
    fromPerception,
    fromCondition,
    fromAction,
    fromSelfAction
) where

import           Smarties.Base


-- $helper1link
-- helpers for building NodeSequenceT out of functions

-- | Utility return utility only
data Utility g p a where
    Utility :: (Num a, Ord a) => (g -> p -> (a, g)) -> Utility g p a
    SimpleUtility :: (Num a, Ord a) => (p -> a) -> Utility g p a

-- | Perception modify pereption only
data Perception g p where
    Perception :: (g -> p -> (g, p)) -> Perception g p
    SimplePerception :: (p -> p) -> Perception g p
    ConditionalPerception :: (g -> p -> (Bool, g, p)) -> Perception g p

-- | Actions create output and always have status SUCCESS
data Action g p o where
    Action :: (g -> p -> (g, o)) -> Action g p o
    SimpleAction :: (p -> o) -> Action g p o

-- | Conditions have status SUCCESS if they return true FAIL otherwise
data Condition g p where
    Condition :: (g -> p -> (Bool, g)) -> Condition g p
    SimpleCondition :: (p -> Bool) -> Condition g p

-- | same as Action except output is applied to perception
data SelfAction g p o where
    SelfAction :: (Reduceable p o) => (g -> p -> (g, o)) -> SelfAction g p o
    SimpleSelfAction :: (Reduceable p o) => (p -> o) -> SelfAction g p o

fromUtility :: (Monad m) => Utility g p a -> NodeSequenceT g p o m a
fromUtility n = NodeSequenceT $ case n of
    Utility f -> func f
    SimpleUtility f -> func (\g p -> (f p, g))
    where
        func f g p = return (a, g', p, SUCCESS, []) where
            (a, g') = f g p

fromPerception :: (Monad m) => Perception g p -> NodeSequenceT g p o m ()
fromPerception n = NodeSequenceT $ case n of
    Perception f -> func f
    SimplePerception f -> func (\g p -> (g, f p))
    ConditionalPerception f -> cfunc f
    where
        func f g p = return ((), g', p', SUCCESS, []) where
            (g', p') = f g p
        cfunc f g p = return ((), g', p', if b then SUCCESS else FAIL, []) where
            (b, g', p') = f g p

fromCondition :: (Monad m) => Condition g p -> NodeSequenceT g p o m ()
fromCondition n = NodeSequenceT $ case n of
    Condition f -> func f
    SimpleCondition f -> func (\g p -> (f p, g))
    where
        func f g p = return ((), g', p, if b then SUCCESS else FAIL, []) where
            (b, g') = f g p

fromAction :: (Monad m) => Action g p o -> NodeSequenceT g p o m ()
fromAction n = NodeSequenceT $ case n of
    Action f -> func f
    SimpleAction f -> func (\g p -> (g, f p))
    where
        func f g p = return ((), g', p, SUCCESS, [o]) where
            (g', o) = f g p

fromSelfAction :: (Monad m) => SelfAction g p o -> NodeSequenceT g p o m ()
fromSelfAction n = NodeSequenceT $ case n of
    SelfAction f -> func f
    SimpleSelfAction f -> func (\g p -> (g, f p))
    where
        func f g p = return ((), g', reduce [o] p, SUCCESS, [o]) where
            (g', o) = f g p
