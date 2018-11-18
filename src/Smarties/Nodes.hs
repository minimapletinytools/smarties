{-|
Module      : Nodes
Description : Various types of generic control nodes
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@gmail.com
Stability   : experimental
-}

module Smarties.Nodes (
    -- $controllink
    --sequence,
    selector,
    weightedSelector,
    utilitySelector,
    utilityWeightedSelector,

    -- $decoratorlink
    flipResult,

    -- $actionlink
    result,
    condition,

    -- $conditionlink
    rand,

    -- ??
    getPerception

) where

import           Prelude                         hiding (sequence)

import           Smarties.Base

import           Control.Applicative.Alternative
import           Control.Lens
import           Control.Monad.Random            hiding (sequence)

import           Data.List                       (find, maximumBy, mapAccumL)
import           Data.Maybe                      (fromMaybe)
import           Data.Ord                        (comparing)

import Debug.Trace (trace)


-- $controllink
-- control nodes

-- | intended use is "sequence $ do"
-- This is prefered over just "do" as it's more explicit.
--sequence :: NodeSequence g p o a -> NodeSequence g p o a
--sequence = id


-- | create a selector node from a list of nodes
-- has monadic return value of the selected child node.
-- you can think of selector as something along the lines of (dropWhile FAIL . take 1)
selector :: [NodeSequence g p o a] -> NodeSequence g p o a
selector ns = NodeSequence func where
    func g p = selected where
        (g',rslts) = mapAccumL mapAccumFn g ns
        mapAccumFn acc x = (acc', r) where
            r = runNodes x acc p
            (_,acc',_,_,_) = r
        selected = fromMaybe (error "selector: all children failed",g',p,FAIL,[]) $
            find (\(_,_,_,x,_)-> x == SUCCESS) rslts

-- internal helper for making weighted selection
weightedSelection ::
    (RandomGen g, Ord w, Random w, Num w)
    => g            -- ^ random generator
    -> [(w,a)]      -- ^ [(weight, element)]
    -> (Maybe a, g) -- ^ selected value and new generator. (Nothing,_) if input list is empty
weightedSelection g ns = if total /= 0 then r else weightedSelection g (zip ([0..]::[Int]) . map snd $ ns) where
    (total, nssummed) = mapAccumL (\acc x -> (acc + fst x, (acc + fst x, snd x))) 0 ns
    (rn, g') = randomR (0, total) g
    r = case find (\(w, _) -> w >= rn) nssummed of
        Just (_,n) -> (Just n, g')
        Nothing    -> (Nothing, g')

-- | create a weighted selector node from a list of nodes and weights
-- this node makes a random weighted selection from its children
-- has monadic return value of the selected child node.
weightedSelector :: (RandomGen g, Ord w, Num w, Random w) => [(w, NodeSequence g p o a)] -> NodeSequence g p o a
weightedSelector ns = NodeSequence func where
    func g p = runNodes selectedNode g' p where
        (msn, g') = weightedSelection g ns
        selectedNode = fromMaybe empty msn

-- | create a utility selector node from a list of nodes
-- this node selects the node with largest weight
-- has monadic return value of the selected child node.
utilitySelector :: (Ord a) => [NodeSequence g p o a] -> NodeSequence g p o a
utilitySelector ns = NodeSequence func where
    func g p = selected where
        (g',rslts) = mapAccumL mapAccumFn g ns
        mapAccumFn acc x = (acc', r) where
            r = runNodes x acc p
            (_,acc',_,_,_) = r
        compfn = (\(a,_,_,_,_)->a)
        selected = if null ns
            then (error "utilitySelector: no children",g',p,FAIL,[])
            else maximumBy (comparing compfn) rslts

-- | create a weighted utility selector node from a list of nodes
-- this node makes a random weighted selection from its children based on their monadic return value
-- has monadic return value of the selected child node.
utilityWeightedSelector :: (RandomGen g, Random a, Num a, Ord a) => [NodeSequence g p o a] -> NodeSequence g p o a
utilityWeightedSelector ns = NodeSequence func where
    func g p = selected where
        (g',rslts) = mapAccumL mapAccumFn g ns
        mapAccumFn acc x = (acc', r) where
            r = runNodes x acc p
            (_,acc',_,_,_) = r
        compelt = (\(a,_,_,_,_)->a)
        (selected', g'') = weightedSelection g' $ map (\x-> (compelt x, x)) rslts
        selected = fromMaybe (error "utilityWeightedSelector: no children",g'',p,FAIL,[]) $ do
            n <- selected'
            return $ set _2 g'' n

-- $decoratorlink
-- decorators run a nodesequence and do something with it's results

-- | create a decorator node that flips the status (FAIL -> SUCCESS, SUCCES -> FAIL)
flipResult :: NodeSequence g p o a -> NodeSequence g p o a
flipResult n = NodeSequence func where
        func g p = over _4 flipr $ runNodes n g p
        flipr s = if s == SUCCESS then FAIL else SUCCESS

-- $actionlink
-- actions
-- | create a status node that has the given status
result :: Status -> NodeSequence g p o ()
result s = NodeSequence (\g p -> ((), g, p, s, []))

-- | create a status node that has the given status
condition :: Bool -> NodeSequence g p o ()
condition s = NodeSequence (\g p -> ((), g, p, if s then SUCCESS else FAIL, []))

-- $conditionlink
-- conditions
-- | create a random status node that has random status based on supplied chance
rand :: (RandomGen g)
    => Float -- ^ chance of success ∈ [0,1]
    -> NodeSequence g p o ()
rand rn = do
    r <- getRandomR (0,1)
    guard (r > rn)
