--so I can do tickTree (SmNodeWrapper (x :: n p o)) ins = (rslt, action) where (rslt, _, action) = update x (BtState (BtStack [empty] :: BtStack m) ins)
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE MultiParamTypeClasses           #-}

--I don't really get this, but it removes warnings for -Wsimplifiable-class-constraints
{-# LANGUAGE MonoLocalBinds #-}


module Smarties.Builder (
    defaultTreeBuilderState,
    SmTreeBuilderFlags(..),
    SmTreeBuilder,
    getTree,
    con,
    dec,
    add,
    addAction,
    addCondition,
    addUtility
) where

import Smarties.Base

import           Control.Exception.Base   (assert)
import           Control.Monad.State.Lazy (State, get, put, runState)
--import System.Random(RandomGen)

data SmTreeBuilderFlags = SmTreeBuilderFlags { makeScope :: Bool }

defaultSmTreeBuilderFlags :: SmTreeBuilderFlags
defaultSmTreeBuilderFlags = SmTreeBuilderFlags True

defaultTreeBuilderState :: ([a], SmTreeBuilderFlags)
defaultTreeBuilderState = ([], defaultSmTreeBuilderFlags)

type SmTreeBuilder p o = State [SmNodeWrapper p o]

getTree :: SmTreeBuilder p o a -> SmNodeWrapper p o
getTree btb = assert (length tree == 1) $ head tree where
    (_, tree) = runState btb []

-- | add 
add :: (SmNode n p o) => n -> SmTreeBuilder p o () 
add n = do
    xs <- get
    put (xs ++ [SmNodeWrapper n])
    return ()

-- consider this option..
-- could also try something like class smaddable?... no that doesn't work :()
addAction :: (SmAction n p o, TreeState p) => n -> SmTreeBuilder p o () 
addAction n = add $ WrapSmAction n

addCondition :: (SmCondition n p, TreeState p) => n -> SmTreeBuilder p o () 
addCondition n = add $ WrapSmCondition n

addUtility :: (SmUtility n p, TreeState p) => n -> SmTreeBuilder p o () 
addUtility n = add $ WrapSmUtility n



-- | control
con :: (SmNode n p o) => ([SmNodeWrapper p o] -> n) -> SmTreeBuilder p o () -> SmTreeBuilder p o ()
con mkn btb = do 
    xs <- get
    let (_, children) = runState btb []
    put (xs ++ [SmNodeWrapper (mkn children)])
    return ()
--branchNoScope :: Proxy n -> SmTreeBuilder p o () -> SmTreeBuilder p o ()

-- | decorate
dec :: (SmNode n p o) => (SmNodeWrapper p o -> n) -> SmTreeBuilder p o () -> SmTreeBuilder p o ()
dec mkn btb = do 
    xs <- get
    let 
        (_, children) = runState btb []
        child = assert (length children == 1) (head children)
    put (xs ++ [SmNodeWrapper (mkn child)])
    return ()
--decorateNoScope

--selector = con Selector
--sequence = con Sequence
--not = dec Not
