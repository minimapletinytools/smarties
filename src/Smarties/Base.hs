{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE AllowAmbiguousTypes     #-}
--{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE UndecidableInstances       #-}

module Smarties.Base (
    module Smarties.TreeState,
    TreeStatus(..),
    Distribution(..),
    Utility(..),
    BaseNodeFlag,
    DerivedNodeFlag,
    SmUtilityNode(..),
    SmNode(..),
    SmNodeWrapper(..),
    SmCondition(..),
    SmAction(..),
    SmUtility(..),
    WrapSmCondition(..),
    WrapSmAction(..),
    WrapSmUtility(..),
    tickTree
) where

import Smarties.TreeState

import           System.Random (RandomGen (..))

data TreeStatus = SUCCESS | FAIL deriving (Eq, Show)


-- | mean stdev
data Distribution = Distribution Float Float
-- TODO lenses or use record syntax

data Utility = Utility Distribution | U_PASS | U_FAIL

data DerivedNodeFlag
data BaseNodeFlag


class (TreeState p) => SmUtilityNode n p where
    utility :: n -> p -> (Utility, p)
    utility _ _ = error "trying to compute utility and a node with no utility"

-- | n is node type
-- | m and p are as in BtInState
-- | p is list of actions to be performed in order.
-- in general, you'll want to fold over the output of type [s] using your game state as an accumulator
-- for example, if game state of type w, p could of type w -> w
class (SmUtilityNode n p) => SmNode n p o where
    -- | node -> input state -> (status, updated state, action)
    update :: n -> p -> (TreeStatus, p, [o]) -- should probably also return a zipper to node that ran
    -- | node -> input state -> (updated state, valid, utility)
    --utility :: n -> p -> (Utility, p)
    --utility _ _ = error "trying to compute utility and a node with no utility"

    -- another option would be jsut to have update/utility to return an INVALID state
    --hasIndex :: (TreeState p) => n x y g o -> p -> Bool
    --hasIndex _ (TreeState (TreeStack ((_,i):_)) _ _) = if i == 0 then True else False
    --hasIndex _ _ = False

-- | helper wrapper class so we can build polymorphic BtNode trees
--data SmNodeWrapper p o = forall n. SmNode n p o => SmNodeWrapper n    
data SmNodeWrapper p o where
    SmNodeWrapper :: (SmNode n p o) => n -> SmNodeWrapper p o

instance (SmUtilityNode (SmNodeWrapper p1 o) p1, TreeState p1, p1~p2) => SmUtilityNode (SmNodeWrapper p1 o) p2 where
    utility (SmNodeWrapper n) p = utility n p
instance (SmNode (SmNodeWrapper p1 o1) p1 o1, TreeState p1, p1~p2, o1~o2) => SmNode (SmNodeWrapper p1 o1) p2 o2 where
    update (SmNodeWrapper n) p = update n p


newtype WrapSmCondition n = WrapSmCondition { unwrapSmCondition :: n }
newtype WrapSmAction n = WrapSmAction { unwrapSmAction :: n }
newtype WrapSmUtility n = WrapSmUtility { unwrapSmUtility :: n }

class (TreeState p) => SmCondition n p where
    condition :: n -> p -> TreeStatus
    -- TODO conditionRand

instance (SmCondition n p, TreeState p) => SmUtilityNode (WrapSmCondition n) p where
    utility n p = case update n p of
        (SUCCESS, nextp, _) -> (U_PASS, nextp)
        (_, nextp, _)       -> (U_FAIL, nextp)

instance (SmCondition n p, TreeState p) => SmNode (WrapSmCondition n) p o where
--    update :: (RandomGen g, TreeStackInfo x) => n -> p -> (TreeStatus, p, [o])
    update n p = ((condition (unwrapSmCondition n) p), p, [])
    

class (TreeState p) => SmAction n p o where
    action :: n -> p -> (TreeStatus, o)
    -- TODO actionRand

instance (TreeState p) => SmUtilityNode (WrapSmAction n) p where
    utility _ p = (U_FAIL, p)

instance (SmAction n p o) => SmNode (WrapSmAction n) p o where
    update n p = (rslt, p, [o]) where
        (rslt, o) = action (unwrapSmAction n) p
    
class SmUtility n p where
    utilityOnly :: n -> p -> Float
    -- TODO utilityRand

instance (SmUtility n p, TreeState p) => SmUtilityNode (WrapSmUtility n) p where
    utility n p = (Utility $ Distribution (utilityOnly (unwrapSmUtility n) p) 0.0, p)

instance (SmUtility n p, TreeState p) => SmNode (WrapSmUtility n) p o where
    update n p = (SUCCESS, p, [])
    

tickTree :: (TreeState p) => SmNodeWrapper p o -> p -> (TreeStatus, p, [o])
tickTree n p = (rslt, nextp, outAction)
    where (rslt, nextp, outAction) = update n p
