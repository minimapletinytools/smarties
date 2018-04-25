{-|
Module      : Builders
Description : Functions and types pertaining to DNA and Genes
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental
-}
module Smarties2.Builders (
    -- $helper1link
    Utility(..),
    Perception(..),
    Action(..),
    Condition(..),
    fromUtility,
    fromPerception,
) where

import           Smarties2.Base


-- $helper1link
-- helpers for building NodeSequence out of functions
data Utility g p = Utility (g -> p -> (Float, g))

data Perception g p = Perception (g -> p -> (g, p))
-- do GADT with multiple constructors
-- SimplePerception :: (p -> p) -> Perception g p
-- Perception :: (g -> p -> (g, p)) -> Perception g p
-- ConditionalPerception :: (g -> p -> (Bool, g, p)) -> Perception g p

data Action g p o = Action (g -> p -> (g, o))

data Condition g p o = Condition (g -> p -> (Bool, g))

fromUtility ::  Utility g p -> NodeSequence g p o Float
fromUtility (Utility n) = NodeSequence func where
    func g p = (u, g', p, SUCCESS, []) where
        (u, g') = n g p

fromPerception :: Perception g p -> NodeSequence g p o ()
fromPerception (Perception n) = NodeSequence func where
    func g p = ((), g', p', SUCCESS, []) where
        (g', p') = n g p


