{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE MultiParamTypeClasses           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE UndecidableInstances       #-}

module NotSoSmarties.Nodes (
    Selector(..),
    Sequence(..),
    UtilitySelector(..),
    UtilityWeightedSelector(..),
    UtilityConst(..),
    UtilityFuncN(..),
    UtilityFunc1(..),
    Not(..),
    Result(..)
) where

import NotSoSmarties.Base
import System.Random (RandomGen, randomR, Random)
import Data.List (mapAccumL, maximumBy, sortOn, find)
import Data.Maybe (Maybe(..), fromJust  )


import Debug.Trace

-- | Selector
data Selector p o where
    Selector :: (TreeState p) => [SmNodeWrapper p o] -> Selector p o


-- | returns Utility of first node with Utility that isn't U_FAIL
-- maybe just have this throw an error. That might be better..
-- TODO problematic because RNG isn't perturbed properly
instance (TreeState p1, p1~p2) => SmUtilityNode (Selector p1 o) p2 where
    utility (Selector b) p = (outU, stackPop outP) where
        (outU, outP) = select' b (stackPush p)
        select' b' p' = case b' of 
            [] -> (U_FAIL, p')
            (n:ns) -> case utility n (p') of
                (Utility d, nextp) -> (Utility d, nextp)
                (U_PASS, nextp)    -> (U_PASS, nextp)
                (U_FAIL, _)  -> select' ns p'

-- | return results of first child node that returns SUCCESS
-- FAIL nodes preceeding the SUCCESS node
-- will have neither their actions nor state mutations applied
-- TODO problematic because RNG isn't perturbed properly
instance (TreeState p1, p1~p2) => SmNode (Selector p1 o) p2 o where
    update (Selector b) p = (outU, stackPop outP, outO) where
        (outU, outP, outO) = select' b (stackPush p)
        select' b' p' = case b' of 
            [] -> (FAIL, p', [])
            (n:ns) -> case update n (p') of
                (SUCCESS, nextp, out) -> (SUCCESS, nextp, out)
                (_, _, _) -> (rslt, stackPop nextp2, out2)
                    where (rslt, nextp2, out2) = select' ns p'         
-- | Sequence
data Sequence p o where
    Sequence :: (TreeState p) => [SmNodeWrapper p o] -> Sequence p o

-- | returns utility of first node with a U_VALID utility status
-- or U_FAIL if it encounters a U_FAIL first
-- TODO problematic because RNG isn't perturbed properly
instance (TreeState p) => SmUtilityNode (Sequence p o) p where
    utility (Sequence b) p = (outU, stackPop outP) where
        origP = stackPush p
        (outU, outP) = sequence' b origP
        sequence' b' p' = case b' of 
            [] -> (U_PASS, p')
            (n:ns) -> case utility n (p') of
                (Utility d, nextp) -> (Utility d, nextp)
                (U_FAIL, _)    -> (U_FAIL, origP)
                (U_PASS, nextp)  -> sequence' ns nextp

-- | returns SUCCESS if all child nodes return SUCCESS including their results in order
-- return FAIL otherwise throwing out all state and action results
-- TODO problematic because RNG isn't perturbed properly
instance (TreeState p1, p1~p2) => SmNode (Sequence p1 o) p2 o where
    update (Sequence b) p = (outU, stackPop outP, outO) where
        origP = stackPush p
        (outU, outP, outO) = sequence' b origP
        sequence' b' p' = case b' of 
            [] -> (SUCCESS, p', [])
            (n:ns) -> case update n (p') of
                (FAIL, nextp, _) -> (FAIL, origP, [])
                (_, nextp, out) -> (rslt, nextp2, out ++ out2)
                    where (rslt, nextp2, out2) = sequence' ns nextp  

                    
-- decide if you want this to return state or not, probably not
-- this should return just a Distribution instead of Utility
utilityMax' :: (TreeState p) => [SmNodeWrapper p o] -> p -> Maybe (Utility, SmNodeWrapper p o)
utilityMax' ns p = if length utils == 0 then Nothing else Just $ maximumBy (compareUtils) utils where
    compareUtils (Utility (Distribution x _),_) (Utility (Distribution y _),_) = compare x y
    compareUtils _ _ = undefined
    utils = utilities' ns p


-- this should return just a Distribution instead of Utility
utilities' :: (TreeState p) => [SmNodeWrapper p o] -> p -> [(Utility, SmNodeWrapper p o)]
utilities' ns p = filter ((\case {Utility f -> True; _ -> False}) . fst) $ utils' where
    (outs, utils') = mapAccumL fn (stackPush p) ns
    fn acc n = (nextp, (u, n)) where
        (u, nextp) = utility n p


weightedRandomSelection :: (RandomGen g, Random a, Num a, Ord a) => g -> [(a, n)] -> Maybe (n, g)
weightedRandomSelection g ns = (,g') . snd <$> find (\(x,_) -> x >= rng) ordered where
    (rng, g') = randomR (0,total) g
    (total, ordered) = mapAccumL (\acc x -> (acc + fst x, (acc + fst x, snd x))) 0 ns

-- | UtilitySelector
data UtilitySelector p o where
    UtilitySelector :: (TreeState p) => [SmNodeWrapper p o] -> UtilitySelector p o

instance (TreeState p) => SmUtilityNode (UtilitySelector p o) p where
    utility (UtilitySelector b) p = case utilityMax' b p of 
        Nothing -> (U_FAIL, p)
        Just (u, _) -> (u, p)


instance (TreeState p) => SmNode (UtilitySelector p o) p o where
    update (UtilitySelector b) p = case utilityMax' b p of 
        Nothing -> (FAIL, p, [])
        Just (_, n) -> update n p



data UtilityWeightedSelector p o where
    UtilityWeightedSelector :: (TreeState p) => [SmNodeWrapper p o] -> UtilityWeightedSelector p o

-- probably better to take average utility here :o
instance (TreeState p) => SmUtilityNode (UtilityWeightedSelector p o) p where
    utility (UtilityWeightedSelector b) p = case utilityMax' b p of 
        Nothing -> (U_FAIL, p)
        Just (u, _) -> (u, p)

instance (TreeState p1, p1~p2, o1~o2) => SmNode (UtilityWeightedSelector p1 o1) p2 o2 where
    update (UtilityWeightedSelector b) p = update choice p where
        (choice, _) = fromJust $ weightedRandomSelection p utils
        utils = map mapUtiltoFloat (utilities' b p)
        mapUtiltoFloat (Utility (Distribution x 0.0), y) = (x,y)
        mapUtiltoFloat _ = undefined



-- | Utility functions

data UtilityConst where
    UtilityConst :: Float -> UtilityConst

instance SmUtility UtilityConst p where
    utilityOnly (UtilityConst u) p = u

--instance SmNode UtilityConst p o

--data UtilityAverage where
--    UtilityAverage

data UtilityFunc1 p o where
    UtilityFunc1 ::  (TreeState p) => (Float->Float) -> SmNodeWrapper p o -> UtilityFunc1 p o

instance (TreeState p1, p1~p2) => SmUtilityNode (UtilityFunc1 p1 o) p2  where
    utility (UtilityFunc1 f n) p =  (nextUtil, nextp) where 
        nextUtil = (\case {Utility (Distribution x _) -> Utility $ Distribution x 0.0; _ -> undefined}) util
        (util, nextp) = utility n p

instance (TreeState p1, p1~p2) => SmNode (UtilityFunc1 p1 o) p2 o where
    update n p = (SUCCESS, p, [])


data UtilityFuncN p o where
    UtilityFuncN ::  (TreeState p) => ([Float]->Float) -> [SmNodeWrapper p o] -> UtilityFuncN p o

-- TODO if this has no children, it should return U_FAIL
instance (TreeState p1, p1~p2) => SmUtilityNode (UtilityFuncN p1 o) p2  where
    utility (UtilityFuncN f ns) p = if length utils > 0 then (Utility $ Distribution (f utils) 0.0, p) else (U_FAIL, p) where
        utils = map (\case {Utility (Distribution x _) -> x; _ -> undefined}) . filter (\case {Utility f -> True; _ -> False}) $ utils'
        (_, utils') = mapAccumL fn (stackPush p) ns
        fn acc n = (nextp, u) where
            (u, nextp) = utility n p

instance (TreeState p1, p1~p2) => SmNode (UtilityFuncN p1 o) p2 o where
    update n p = (SUCCESS, p, [])



-- | Decorators

-- | Not has one child and returns
data Not p o where
    Not :: (TreeState p) => SmNodeWrapper p o -> Not p o

instance (TreeState p1, p1~p2) => SmUtilityNode (Not p1 o) p2

instance (TreeState p1, p1~p2, o1~o2) => SmNode (Not p1 o1) p2 o2 where
    update (Not n) p = case update n (stackPush p) of
        (SUCCESS, nextp, out) -> (FAIL, stackPop nextp, out)
        (FAIL, nextp, out)    -> (SUCCESS, stackPop nextp, out)

data Rand p o where 
    Rand :: (TreeState p) => Float -> SmNodeWrapper p o -> Rand p o

instance (TreeState p1, p1~p2) => SmUtilityNode (Rand p1 o) p2

instance (TreeState p1, p1~p2, o1~o2) => SmNode (Rand p1 o1) p2 o2 where
    update (Rand r n) p = if (rn < r) then (update n nextp) else (FAIL, nextp, []) where
        (rn, nextp) = randomR (0.0, 1.0) p


-- | Conditions / Actions

-- | Result returns the specified result
data Result where
    Result :: TreeStatus -> Result

instance (TreeState p) => SmCondition Result p where
    condition (Result x) _ = x
    --update (Result s) btsi = (s, btsi, [])
