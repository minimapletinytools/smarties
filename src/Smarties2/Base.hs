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
module Smarties2.Base (
	Status(..),
	NodeSequence(..),
	runNodeSequence,
	getPerception,
	getGenerator,
	setGenerator
	-- $helperlink
) where	

import Smarties2.TreeState

import Control.Lens
import Control.Monad.Random
import Control.Applicative.Alternative




--https://ccrma.stanford.edu/~jos/sasp/Product_Two_Gaussian_PDFs.html
--https://en.wikipedia.org/wiki/Sum_of_normally_distributed_random_variables

data Status = SUCCESS | FAIL deriving (Eq, Show)

-- |
-- TODO add a (scope :: Bool) input parameter
data NodeSequence g p o a =  NodeSequence { runNodes :: g -> p -> (a, g, p, Status, [o]) }

runNodeSequence :: NodeSequence g p o a -> g -> p -> (g, p, Status, [o])
runNodeSequence n g p = (\(_,g,p,s,os)->(g,p,s,os)) $ (runNodes n) g p

-- $helperlink
-- helpers for building NodeSequence in Monad land

-- | returns the perception state
getPerception :: NodeSequence g p o p
getPerception = NodeSequence $ (\g p -> (p, g, p, SUCCESS, []))

-- | returns the generator 
getGenerator ::  NodeSequence g p o g
getGenerator = NodeSequence $ (\g p -> (g, g, p, SUCCESS, []))

-- | set the generator in the monad
setGenerator :: (RandomGen g) => g -> NodeSequence g p o ()
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
			keepGoing = over _5 (os++) (n' g' p') -- run the next node
			(b,_,_,_,_) = keepGoing	

instance (RandomGen g) => MonadRandom (NodeSequence g p o) where
	--getRandoms = iterate getRandom
    --getRandomRs r = iterate (getRandomR r)
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