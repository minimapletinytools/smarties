{-|
Module      : Nodes
Description : Functions and types pertaining to DNA and Genes
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental
-}
module Smarties2.Nodes (
    -- $controllink
    sequence,
    selector,
    weightedSelector,
    utilitySelector,
    utilityWeightedSelector,

    -- $decoratorlink
    flipResult,

    -- $actionlink
    result,

    -- $conditionlink
    rand

) where

import           Prelude                         hiding (sequence)

import           Smarties2.Base
import           Smarties2.TreeState

import           Control.Applicative.Alternative
import           Control.Lens
import           Control.Monad.Random            hiding (sequence)

import           Data.List                       (find, maximumBy, mapAccumL)
import           Data.Maybe                      (fromMaybe)
import           Data.Ord                        (comparing)


-- $controllink
-- control nodes

-- | this is same as "do" except scopes the perception
sequence :: (TreeState p) => NodeSequence g p o a -> NodeSequence g p o a
sequence ns = NodeSequence func where
    func g p = over _3 stackPop $ (runNodes ns) g (stackPush p)



-- |
-- TODO replace with mapAccumL because need to accumulate p and g
-- you can think of selector as something along the lines of (dropWhile SUCCESS . take 1)
selector :: (TreeState p) => [NodeSequence g p o a] -> NodeSequence g p o a
selector ns = NodeSequence func where
    func g p = selected where
        (g',rslts) = mapAccumL mapAccumFn g ns
        mapAccumFn acc x = (acc', r) where
            r = (runNodes x) acc (stackPush p)
            (_,acc',_,_,_) = r
        selected = fromMaybe (error "selector: all children failed",g',p,FAIL,[])  $ do
            n <- find (\(_,_,_,x,_)-> x == SUCCESS) rslts
            return $ over _3 stackPop n

-- |
weightedSelection :: (RandomGen g, Ord w, Random w, Num w) => g -> [(w,a)] -> (Maybe a, g)
weightedSelection g ns = r where
    zero = fromInteger 0
    total = foldl (\acc x -> fst x + acc) zero ns
    (rn, g') = randomR (zero, total) g
    r = case find (\(w, _) -> w >= rn) ns of
        Just (_,n) -> (Just n, g')
        Nothing    -> (Nothing, g')

-- |
weightedSelector :: (RandomGen g, TreeState p, Ord w, Num w, Random w) => [(w, NodeSequence g p o a)] -> NodeSequence g p o a
weightedSelector ns = NodeSequence func where
    func g p = over _3 stackPop $ (runNodes selectedNode) g' (stackPush p) where
        (msn, g') = weightedSelection g ns
        selectedNode = fromMaybe empty msn

-- |
utilitySelector :: (TreeState p, Ord a) => [NodeSequence g p o a] -> NodeSequence g p o a
utilitySelector ns = NodeSequence func where
    func g p = selected where
        (g',rslts) = mapAccumL mapAccumFn g ns
        mapAccumFn acc x = (acc', r) where
            r = (runNodes x) acc (stackPush p)
            (_,acc',_,_,_) = r
        compfn = (\(a,_,_,_,_)->a)
        selected = if length ns == 0 
            then (error "utilitySelector: no children",g',p,FAIL,[]) 
            else maximumBy (comparing compfn) rslts

-- |
utilityWeightedSelector :: (RandomGen g, TreeState p, Random a, Num a, Ord a) => [NodeSequence g p o a] -> NodeSequence g p o a
utilityWeightedSelector ns = NodeSequence func where
    func g p = selected where
        (g',rslts) = mapAccumL mapAccumFn g ns
        mapAccumFn acc x = (acc', r) where
            r = (runNodes x) acc (stackPush p)
            (_,acc',_,_,_) = r
        compfn = (\(a,_,_,_,_)->a)
        (selected', g'') = weightedSelection g' $ map (\x-> (compfn x,x)) rslts
        selected = fromMaybe (error "utilitySelector: no children",g'',p,FAIL,[]) $ do
            n <- selected'
            return $ set _2 g'' n

-- $decoratorlink
-- decorators run a nodesequence and do something with it's results

-- | decorator that flips the status (FAIL -> SUCCESS, SUCCES -> FAIL)
flipResult :: NodeSequence g p o a -> NodeSequence g p o a
flipResult n = NodeSequence func where
        func g p = over _4 flipr $ (runNodes n) g p
        flipr s = if s == SUCCESS then FAIL else SUCCESS

-- $actionlink
-- actions
-- | has given status
result :: Status -> NodeSequence g p o ()
result s = NodeSequence (\g p -> ((), g, p, s, []))

-- $conditionlink
-- conditions
-- | has random status based on supplied chance
rand :: (RandomGen g) => Float -- ^ chance of success ∈ [0,1]
    -> NodeSequence g p o ()
rand rn = do
    r <- getRandomR (0,1)
    guard (r > rn)
