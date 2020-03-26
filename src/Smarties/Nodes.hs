{-|
Module      : Nodes
Description : MTL equivalent of Smarties.Nodes
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

  -- $conditionlink
  result,
  condition,
  rand

) where

import           Prelude                         hiding (sequence)

import           Smarties.Base

import           Control.Applicative
import           Lens.Micro
import           Control.Monad.Random            hiding (sequence)

import           Data.List                       (find, mapAccumL, maximumBy)
import           Data.Maybe                      (fromMaybe)
import           Data.Ord                        (comparing)


-- $controllink
-- control nodes

-- | intended use is "sequence $ do"
-- This is prefered over just "do" as it's more explicit.
--sequence :: NodeSequenceT g p o m a -> NodeSequenceT g p o m a
--sequence = id

-- monadic mapAccumR
mapAccumRM :: (Monad m) => (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, [y])
mapAccumRM f acc_ xs = foldr mapAccumM_ (return (acc_, [])) xs where
  mapAccumM_ x prev = do
    (acc, ys) <- prev
    (acc', y) <- f acc x
    return (acc', ys ++ [y])

-- run a node sequence and return its accumulated generator
-- this is used to mapAccumR over [NodeSequenceT] passing generator through
-- TODO rename this function
mapAccumNodeSequenceT :: (Monad m) => p -> g -> NodeSequenceT g p o m a -> m (g, (a, g, p, Status, [o]))
mapAccumNodeSequenceT p acc x = do
  r <- (runNodes x) acc p
  let (_,acc',_,_,_) = r
  return (acc', r)


-- you can think of selector as something along the lines of (dropWhile SUCCESS . take 1)
selector :: (Monad m) => [NodeSequenceT g p o m a] -> NodeSequenceT g p o m a
selector ns = NodeSequenceT func where
  func g p = do
    (g', rslts) <- mapAccumRM (mapAccumNodeSequenceT p) g ns
    return $ fromMaybe (error "selector: all children failed",g',p,FAIL,[]) $
      find (\(_,_,_,x,_)-> x == SUCCESS) rslts


-- |
weightedSelection :: (RandomGen g, Ord w, Random w, Num w) => g -> [(w,a)] -> (Maybe a, g)
weightedSelection g ns = if total /= 0 then r else weightedSelection g (zip ([0..]::[Int]) . map snd $ ns) where
  (total, nssummed) = mapAccumL (\acc x -> (acc + fst x, (acc + fst x, snd x))) 0 ns
  (rn, g') = randomR (0, total) g
  r = case find (\(w, _) -> w >= rn) nssummed of
    Just (_,n) -> (Just n, g')
    Nothing    -> (Nothing, g')

-- |
weightedSelector :: (RandomGen g, Ord w, Num w, Random w, Monad m) => [(w, NodeSequenceT g p o m a)] -> NodeSequenceT g p o m a
weightedSelector ns = NodeSequenceT func where
  func g p = (runNodes selectedNode) g' p where
    (msn, g') = weightedSelection g ns
    selectedNode = fromMaybe empty msn

-- | it's easy to forget that utility must be the last monadic return value of a NodeSequence to be understood by utility selectors
-- this type family is used to prevent accidental usage of `instance Ord ()` in utility selectors
type family NotUnit a where
  NotUnit () = 'False
  NotUnit a = 'True

-- | returns the node sequence with maximum utility
-- N.B. that this will dry execute ALL node sequences in the input list so be mindful of performance
-- However any side effects will not be reverted either so avoid Nodes with mutable writes inside of utilitySelector!!!
utilitySelector :: (Ord a, NotUnit a ~ 'True, Monad m) => [NodeSequenceT g p o m a] -> NodeSequenceT g p o m a
utilitySelector ns = NodeSequenceT func where
  func g p = do
    (g', rslts) <- mapAccumRM (mapAccumNodeSequenceT p) g ns
    let compfn = (\(a,_,_,_,_)-> a)
    if null ns
      then return (error "utilitySelector: no children",g',p,FAIL,[])
      else return $ maximumBy (comparing compfn) rslts

-- |
utilityWeightedSelector :: (RandomGen g, Random a, Num a, Ord a, NotUnit a ~ 'True, Monad m) => [NodeSequenceT g p o m a] -> NodeSequenceT g p o m a
utilityWeightedSelector ns = NodeSequenceT func where
  func g p = do
    (g', rslts) <- mapAccumRM (mapAccumNodeSequenceT p) g ns
    let
      compelt = (\(a,_,_,_,_)->a)
      (selected', g'') = weightedSelection g' $ map (\x-> (compelt x, x)) rslts
    return $ fromMaybe (error "utilityWeightedSelector: no children",g'',p,FAIL,[]) $ do
        n <- selected'
        return $ set _2 g'' n

-- $decoratorlink
-- decorators run a nodesequence and do something with it's results

-- | decorator that flips the status (FAIL -> SUCCESS, SUCCES -> FAIL)
flipResult :: (Monad m) => NodeSequenceT g p o m a -> NodeSequenceT g p o m a
flipResult n = NodeSequenceT func where
    flipr s = if s == SUCCESS then FAIL else SUCCESS
    func g p = do
      rslt <- runNodes n g p
      return $ over _4 flipr rslt

-- this is fine and all except if this occurs after a FAILed NodeSequence it will still output so make sure you clearly document that this is the case and why
--traceNode :: (Monad m) => String -> NodeSequenceT g p o m ()
--traceNode msg = NodeSequenceT (\g p -> trace msg $ return ((), g, p, SUCCESS, []))

-- $conditionlink
-- conditions
-- | has given status
result :: (Monad m) => Status -> NodeSequenceT g p o m ()
result s = NodeSequenceT (\g p -> return ((), g, p, s, []))

-- | create a condition node, SUCCESS if true FAIL otherwise
condition :: (Monad m) => Bool -> NodeSequenceT g p o m ()
condition s = NodeSequenceT (\g p -> return ((), g, p, if s then SUCCESS else FAIL, []))

-- | create a node with random status based on input chance
rand :: (RandomGen g, Monad m) => Float -- ^ chance of success ∈ [0,1]
  -> NodeSequenceT g p o m ()
rand rn = do
  r <- getRandomR (0,1)
  guard (r > rn)
