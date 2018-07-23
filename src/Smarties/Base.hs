{-|
Module      : Base
Description : Functions and types pertaining to DNA and Genes
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental
-}

--{-# LANGUAGE ApplicativeDo #-}
--{-# RankNTypes #-}
module Smarties.Base (
    Reduceable(..),
    Status(..),
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
import Control.Applicative.Alternative




--https://ccrma.stanford.edu/~jos/sasp/Product_Two_Gaussian_PDFs.html
--https://en.wikipedia.org/wiki/Sum_of_normally_distributed_random_variables

class Reduceable p o where
    reduce :: [o] -> p -> p

--probably {-# OVERLAPPABLE #-}
instance Reduceable a (a->a) where
    reduce os = foldr (.) id os


data Status = SUCCESS | FAIL deriving (Eq, Show)

-- |
-- TODO add a (scope :: Bool) input parameter
data NodeSequence g p o a =  NodeSequence { runNodes :: g -> p -> (a, g, p, Status, [o]) }

-- | run a node sequence tossing its monadic output
-- output is ordered from RIGHT to LEFT i.e. foldr when applying
execNodeSequence :: NodeSequence g p o a -> g -> p -> (g, p, Status, [o])
execNodeSequence n _g _p = (\(_,g,p,s,os)->(g,p,s,os)) $ (runNodes n) _g _p

-- | internal helper
iterate_ :: Int -> (a -> a) -> a -> a
iterate_ n f = foldr (.) id (replicate n f)

-- | run a node sequence several times using its output to generate the next perception state
execNodeSequenceTimes :: (Reduceable p o) => Int -> NodeSequence g p o a -> g -> p -> (g, p, Status, [o])
execNodeSequenceTimes num n _g _p = iterate_ num itfun (_g, _p, SUCCESS, []) where
    itfun (g,p,_,os) = execNodeSequence n g (reduce os p)

-- | same as runNodeSequenceTimes except reduces the final input with its output and only returns this result
execNodeSequenceTimesFinalize :: (Reduceable p o) => Int -> NodeSequence g p o a -> g -> p -> p
execNodeSequenceTimesFinalize num n _g _p = reduce os p where
    (_,p,_,os) = execNodeSequenceTimes num n _g _p


-- $helperlink
-- helpers for building NodeSequence in Monad land

-- | returns the perception state
getPerception :: NodeSequence g p o p
getPerception = NodeSequence $ (\g p -> (p, g, p, SUCCESS, []))

-- | sets the perception state
setPerception :: p -> NodeSequence g p o ()
setPerception p' = NodeSequence $ (\g _ -> ((), g, p', SUCCESS, []))

-- | add to output
tellOutput :: o -> NodeSequence g p o ()
tellOutput o = NodeSequence $ (\g p -> ((), g, p, SUCCESS, [o]))

-- | returns the generator
getGenerator ::  NodeSequence g p o g
getGenerator = NodeSequence $ (\g p -> (g, g, p, SUCCESS, []))

-- | set the generator in the monad
setGenerator :: g -> NodeSequence g p o ()
setGenerator g = NodeSequence $ (\_ p -> ((), g, p, SUCCESS, []))


-- instance declarations for NodeSequence
-- helpers for building NodeSequence in Monad land

instance  Functor (NodeSequence g p o) where
    fmap f n = do
        a <- n
        return $ f a

instance  Applicative (NodeSequence g p o) where
    pure a = NodeSequence (\g p -> (a, g, p, SUCCESS, []))
    -- we should do something naughty here instead of reusing >>=
    liftA2 f n1 n2 = do
        a <- n1
        b <- n2
        return $ f a b

instance Alternative (NodeSequence g p o) where
    --empty :: NodeSequence g p o a
    empty = NodeSequence func where
        func g p = (error "trying to pull value from a guard", g, p, FAIL, [])
        a <|> b = a >>= \_ -> b

-- |
-- note this looks a lot like (StateT (g,p) Writer o) but has special functionality built in
-- note, I'm pretty sure this does not satisfy monad laws
instance  Monad (NodeSequence g p o) where
    -- should only ever be used by sequence type nodes
    (>>=) :: NodeSequence g p o a -> (a -> NodeSequence g p o b) -> NodeSequence g p o b
    NodeSequence n >>= f = NodeSequence func where
        -- if we fail, abort the update but pass on the output and state vars
        -- otherwise keep going
        func g p = if s == FAIL then (b, g', p, FAIL, os) else keepGoing where
            (a, g', p', s, os) = n g p -- run original node, assume it succeded
            NodeSequence n' = f a -- generate the next node
            keepGoing = over _5 (++os) (n' g' p') -- run the next node
            (b,_,_,_,_) = keepGoing

instance (RandomGen g) => MonadRandom (NodeSequence g p o) where
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
