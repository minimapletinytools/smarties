{-# LANGUAGE ApplicativeDo #-}
{-# RankNTypes #-}
module Smarties2.Base (
) where	

import Smarties2.TreeState

import Control.Lens
import Control.Monad.Random
import Control.Applicative.Alternative

import Data.Maybe (fromMaybe)
import Data.List (find)




--https://ccrma.stanford.edu/~jos/sasp/Product_Two_Gaussian_PDFs.html
--https://en.wikipedia.org/wiki/Sum_of_normally_distributed_random_variables

data Status = SUCCESS | FAIL deriving (Eq, Show)

data UtilityNode g p o = UtilityNode {
	utility :: (RandomGen g) => g -> p -> (Float, g)
}

data PerceptionNode g p o = PerceptionNode {
	perception :: (RandomGen g) => g -> p -> (g, p)
}

data ActionNode g p o =  ActionNode {
	action :: (RandomGen g) => g -> p -> (g, o)	
}

makeUtility :: UtilityNode p o -> NodeSequence g p o Float
makeUtility (UtilityNode n) = NodeSequence fun where
	fun g p = (u, g', SUCCESS, p, []) where
		(u, g') = n g p

data NodeSequence g p o a = NodeSequence { runNodes :: g -> p -> (a, g, Status, p, [o]) }

-- this looks a lot like StateT Writer
instance Monad (NodeSequence g p o) where
	-- should only ever be used by sequence type nodes
	NodeSequence n >>= f = NodeSequence func where
		-- if we fail, abort the update but pass on the output and state vars
		-- otherwise keep going
		func g p = if s == FAIL then (a', g', FAIL, p, os) else keepGoing where 
			(a, g', s, p', os) = n g p -- run original node, assume it succeded
			NodeSequence n' = f a -- generate the next node
			keepGoing = over _4 (++os) (n' g' p') -- run the next node
			(a',_,_,_) = keepGoing
			
	

{-instance (RandomGen g) => MonadRandom (NodeSequence g p o) where
    getRandom = do
    	g <- getGenerator
    	let (a, g') = random g
    	setGenerator g'
    	return a    
    getRandomR r = do
    	g <- getGenerator
    	let (a, g') = randomR r g
    	setGenerator g'
    	return a
	--getRandoms = getGenerator >>= iterate getRandom
    --getRandomRs r = getGenerator >>= iterate (getRandomR r)
    -}


instance Alternative (NodeSequence g p o) where
	--empty :: NodeSequence g p o a
	empty = NodeSequence func where
		func g p = (error "trying to pull value from a guard", g, FAIL, p, [])
	a <|> b = a >>= \_ -> b

instance Functor (NodeSequence g p o) where
	fmap f n = do 
		a <- n
		return $ f a

instance Applicative (NodeSequence g p o) where
	pure a = NodeSequence (\g p -> (a, g, SUCCESS, p, []))
	-- we should do something naughty here instead of reusing >>=
	liftA2 f n1 n2 = do 
		a <- n1
		b <- n2
		return $ f a b


getState :: NodeSequence g p o p
getState = NodeSequence $ (\g p -> (p, g, SUCCESS, p, []))

--getGenerator :: (RandomGen g) => NodeSequence g p o g
--getGenerator = NodeSequence $ (\g p -> (g, g, SUCCESS, p, []))

--setGenerator :: (RandomGen g) => g -> NodeSequence () p o p
--setGenerator g = NodeSequence $ (\_ p -> ((), g, SUCCESS, p, []))


makeSequence :: NodeSequence g p o a -> NodeSequence g p o a 
makeSequence = id

selector :: [NodeSequence g p o a] -> NodeSequence g p o a
selector ns = NodeSequence fun where 
	fun g p = fromMaybe empty $ find (\NodeSequence n -> n p g == (_,_,SUCCESS,_,_))

{-
makeSequence $ do
	selectSomething
	conditionSomethingSelected
	a <- utilityA
	b <- utilityb
	doSomething 
	return makeUtility (a+b) -- utility of this sequence
	-}