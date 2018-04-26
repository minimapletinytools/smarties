{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module NotSoSmarties.BuilderNodes (
    selector,
    sequence,
    utilitySelector,
    utilityWeightedSelector,
    utilityConst,
    utilityMultiply,
    utilityAverage,
    utilityOneMinus,
    dNot,
    rSuccess,
    rFail
    --smResult
) where

import Prelude hiding (sequence, not, fail)
import NotSoSmarties.Base
import NotSoSmarties.TreeState()
import NotSoSmarties.Builder
import NotSoSmarties.Nodes

selector :: (TreeState p) => SmTreeBuilder p o () -> SmTreeBuilder p o ()
selector = con Selector

sequence :: (TreeState p) => SmTreeBuilder p o () -> SmTreeBuilder p o ()
sequence = con Sequence

utilitySelector :: (TreeState p) => SmTreeBuilder p o () -> SmTreeBuilder p o ()
utilitySelector = con UtilitySelector

utilityWeightedSelector :: (TreeState p) => SmTreeBuilder p o () -> SmTreeBuilder p o ()
utilityWeightedSelector = con UtilityWeightedSelector

utilityConst :: (TreeState p) => Float -> SmTreeBuilder p o ()
utilityConst = addUtility . UtilityConst 

-- TODO restrict argument to be [0,1]
utilityMultiply :: (TreeState p) => SmTreeBuilder p o () -> SmTreeBuilder p o ()
utilityMultiply = con . UtilityFuncN $ foldl1 (*) 

utilityAverage :: (TreeState p) => SmTreeBuilder p o () -> SmTreeBuilder p o ()
utilityAverage = con . UtilityFuncN $ (\xs -> sum xs / fromIntegral (length xs))

utilityOneMinus :: (TreeState p) => SmTreeBuilder p o () -> SmTreeBuilder p o ()
utilityOneMinus = dec . UtilityFunc1 $ (1-)

dNot  :: (TreeState p) => SmTreeBuilder p o () -> SmTreeBuilder p o ()
dNot = dec Not

-- what we really want here is some template haskell that takes ctor of a SmAction/SmCondition
 -- and generates smActionOrCondition :: <ctor params> -> SmTreeBuilder p o ()
--smResult :: SmTreeBuilder p o () 
--smResult = add Result

rSuccess :: (TreeState p) => SmTreeBuilder p o ()
rSuccess = addCondition (Result SUCCESS)

rFail :: (TreeState p) => SmTreeBuilder p o ()
rFail = addCondition (Result FAIL)