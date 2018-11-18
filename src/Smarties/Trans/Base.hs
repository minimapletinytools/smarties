{-|
Module      : Base
Description : MTL equivalent of Smarties.Base
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@gmail.com
Stability   : experimental
-}

module Smarties.Trans.Base (
    Reduceable(..),
    Status(..),
    NodeSequenceT(..),
    execNodeSequenceT,
    execNodeSequenceTimesT,
    execNodeSequenceTimesFinalizeT,
    NodeSequence(..),
    execNodeSequence,
    execNodeSequenceTimes,
    execNodeSequenceTimesFinalize,
    getPerception,
    setPerception,
    tellOutput,
    getGenerator,
    setGenerator
    -- $helperlink
) where

import Control.Lens
import Control.Monad.Random
import Control.Monad.Identity (Identity, runIdentity)
import Control.Applicative.Alternative




--https://ccrma.stanford.edu/~jos/sasp/Product_Two_Gaussian_PDFs.html
--https://en.wikipedia.org/wiki/Sum_of_normally_distributed_random_variables

class Reduceable p o where
    reduce :: [o] -> p -> p

-- probably {-# OVERLAPPABLE #-}
instance Reduceable a (a->a) where
    reduce os = foldr (.) id os


data Status = SUCCESS | FAIL deriving (Eq, Show)

-- |
newtype NodeSequenceT g p o m a =  NodeSequenceT { runNodes :: g -> p -> m (a, g, p, Status, [o]) }

-- | run a node sequence tossing its monadic output
-- output is ordered from RIGHT to LEFT i.e. foldr when applying
execNodeSequenceT :: (Monad m) => NodeSequenceT g p o m a -> g -> p -> m (g, p, Status, [o])
execNodeSequenceT n g p = (runNodes n) g p >>= (\(_,g,p,s,os) -> return (g,p,s,os))

-- | internal helper
iterate_ :: (Monad m) => Int -> (a -> m a) -> a -> m a
iterate_ n f = foldr (>=>) return (replicate n f)

-- | run a node sequence several times using its output to generate the next perception state
execNodeSequenceTimesT :: (Reduceable p o, Monad m) => Int -> NodeSequenceT g p o m a -> g -> p -> m (g, p, Status, [o])
execNodeSequenceTimesT num n _g _p = iterate_ num itfun (_g, _p, SUCCESS, []) where
    itfun (g,p,_,os) = execNodeSequenceT n g (reduce os p)

-- | same as runNodeSequenceTimes except reduces the final input with its output and only returns this result
execNodeSequenceTimesFinalizeT :: (Reduceable p o, Monad m) => Int -> NodeSequenceT g p o m a -> g -> p -> m p
execNodeSequenceTimesFinalizeT num n _g _p = do
    (_,p,_,os) <- execNodeSequenceTimesT num n _g _p
    return $ reduce os p

-- $nontransformerlink

-- | has the exact same interface as the one in Smarties.Base
type NodeSequence g p o a = NodeSequenceT g p o Identity a

-- |
execNodeSequence :: NodeSequence g p o a -> g -> p -> (g, p, Status, [o])
execNodeSequence n g p = runIdentity $ execNodeSequenceT n g p

-- |
execNodeSequenceTimes :: (Reduceable p o) => Int -> NodeSequence g p o a -> g -> p -> (g, p, Status, [o])
execNodeSequenceTimes num n g p = runIdentity $ execNodeSequenceTimesT num n g p

-- |
execNodeSequenceTimesFinalize :: (Reduceable p o) => Int -> NodeSequence g p o a -> g -> p -> p
execNodeSequenceTimesFinalize num n g p = runIdentity $ execNodeSequenceTimesFinalizeT num n g p

-- $helperlink
-- helpers for building NodeSequence in Monad land

-- | returns the perception state
getPerception :: (Monad m) => NodeSequenceT g p o m p
getPerception = NodeSequenceT $ (\g p -> return (p, g, p, SUCCESS, []))

-- | sets the perception state
setPerception :: (Monad m) => p -> NodeSequenceT g p o m ()
setPerception p' = NodeSequenceT $ (\g p -> return ((), g, p', SUCCESS, []))

-- | add to output
tellOutput :: (Monad m) => o -> NodeSequenceT g p o m ()
tellOutput o = NodeSequenceT $ (\g p -> return ((), g, p, SUCCESS, [o]))

-- | returns the generator
getGenerator :: (Monad m) => NodeSequenceT g p o m g
getGenerator = NodeSequenceT $ (\g p -> return (g, g, p, SUCCESS, []))

-- | set the generator in the monad
setGenerator :: (RandomGen g, Monad m) => g -> NodeSequenceT g p o m ()
setGenerator g = NodeSequenceT $ (\_ p -> return ((), g, p, SUCCESS, []))


-- instance declarations for NodeSequence
-- helpers for building NodeSequence in Monad land

-- |
-- it's possible to do this without Monad m restriction, but reusing >>= is better
instance (Functor m, Monad m) => Functor (NodeSequenceT g p o m) where
    fmap :: (a -> b) -> NodeSequenceT g p o m a -> NodeSequenceT g p o m b
    fmap f n = do
        a <- n
        return $ f a
    --fmap f n = NodeSequenceT func where
    --    func g_ p_ = fmap f' ((runNodes n) g_ p_) where
    --        f' (a, g, p, s, os) = (f a, g, p, s, os)


-- |
-- it's possible to do this without Monad m restriction, but reusing >>= is better
instance (Applicative m, Monad m) => Applicative (NodeSequenceT g p o m) where
    pure a = NodeSequenceT (\g p -> pure (a, g, p, SUCCESS, []))
    liftA2 f n1 n2 = do
        a <- n1
        b <- n2
        return $ f a b

instance (Applicative m, Monad m) => Alternative (NodeSequenceT g p o m) where
    --empty :: NodeSequenceT g p o m a
    empty = NodeSequenceT func where
        func g p = return (error "trying to pull value from a guard", g, p, FAIL, [])
    a <|> b = a >>= \_ -> b

-- |
-- note this looks a lot like (StateT (g,p) Writer o) but has special functionality built in
-- note, I'm pretty sure this does not satisfy monad laws
instance (Monad m) => Monad (NodeSequenceT g p o m) where
    -- should only ever be used by sequence type nodes
    (>>=) :: NodeSequenceT g p o m a -> (a -> NodeSequenceT g p o m b) -> NodeSequenceT g p o m b
    NodeSequenceT n >>= f = NodeSequenceT func where
        -- if we fail, abort the update but pass on the output and state vars
        -- otherwise keep going
        func g p = do
            (a, g', p', s, os) <- n g p
            let
                NodeSequenceT n' = f a -- generate the next node
            rslt <- (n' g' p') -- run the next node
            let
                keepGoing = over _5 (++os) rslt
                (b,_,_,_,_) = keepGoing
            if s == FAIL then return (b, g', p, FAIL, os) else return keepGoing where


instance MonadTrans (NodeSequenceT g p o) where
    lift m = NodeSequenceT (\g p -> m >>= (\a -> return (a, g, p, SUCCESS,[])))


instance (RandomGen g, Monad m) => MonadRandom (NodeSequenceT g p o m) where
    -- TODO
    getRandoms = undefined
    getRandomRs _ = undefined
    getRandom = do
        g <- getGenerator
        let
            (a, g') = random g
        setGenerator g'
        return a
    getRandomR r = do
        g <- getGenerator
        let
            (a, g') = randomR r g
        setGenerator g'
        return a
