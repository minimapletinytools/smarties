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
	utility :: g -> p -> (Float, g)
}

data PerceptionNode g p o = PerceptionNode {
	perception :: g -> p -> (g, p)
}

data ActionNode g p o = ActionNode {
	action :: g -> p -> (g, o)	
}

makeUtility ::  UtilityNode g p o -> NodeSequence g p o Float
makeUtility (UtilityNode n) = NodeSequence fun where
	fun g p = (u, g', p, SUCCESS, []) where
		(u, g') = n g p

-- |
-- TODO add a (scope :: Bool) input parameter
data NodeSequence g p o a =  NodeSequence { runNodes :: g -> p -> (a, g, p, Status, [o]) }

-- this looks a lot like StateT Writer
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
    


instance Alternative (NodeSequence g p o) where
	--empty :: NodeSequence g p o a
	empty = NodeSequence func where
		func g p = (error "trying to pull value from a guard", g, p, FAIL, [])
	a <|> b = a >>= \_ -> b

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

-- | NodeSequence builder helper 
getState ::   NodeSequence g p o p
getState = NodeSequence $ (\g p -> (p, g, p, SUCCESS, []))

-- | NodeSequence builder helper 
getGenerator ::  NodeSequence g p o g
getGenerator = NodeSequence $ (\g p -> (g, g, p, SUCCESS, []))

setGenerator :: (RandomGen g) => g -> NodeSequence g p o ()
setGenerator g = NodeSequence $ (\_ p -> ((), g, p, SUCCESS, []))


sequence :: (TreeState p) => NodeSequence g p o a -> NodeSequence g p o a 
sequence ns = NodeSequence func where
		func g p = over _3 stackPop $ (runNodes ns) g (stackPush p)

selector :: (TreeState p) => [NodeSequence g p o a] -> NodeSequence g p o a
selector ns = NodeSequence func where 
	func g p = over _3 stackPop $ (runNodes selectedNode) g (stackPush p) where
		selectedNode = fromMaybe empty $ find (\(NodeSequence n) -> (\case (_,_,_,x,_)-> x == SUCCESS) $ n g p) ns

weightedSelection :: (RandomGen g, Ord w, Random w, Num w) => g -> [(w,a)] -> (Maybe a, g)
weightedSelection g ns = r where
	zero = fromInteger 0
	total = foldl (\acc x -> fst x + acc) zero ns 
	(rn, g') = randomR (zero, total) g
	r = case find (\(w, _) -> w >= rn) ns of
		Just (_,n) -> (Just n, g') 
		Nothing -> (Nothing, g')

weightedSelector :: (RandomGen g, TreeState p, Ord w, Num w, Random w) => [(w, NodeSequence g p o a)] -> NodeSequence g p o a
weightedSelector ns = NodeSequence func where  
	func g p = over _3 stackPop $ (runNodes selectedNode) g' (stackPush p) where
		(msn, g') = weightedSelection g ns
		selectedNode = fromMaybe empty msn

result :: Status -> NodeSequence g p o ()
result s = NodeSequence (\g p -> ((), g, p, s, []))

flipResult :: NodeSequence g p o a -> NodeSequence g p o a
flipResult n = NodeSequence func where
		func g p = over _4 flipr $ (runNodes n) g p 
		flipr s = if s == SUCCESS then FAIL else SUCCESS

rand :: (RandomGen g) => Float -> NodeSequence g p o ()
rand rn = do
	r <- getRandomR (0,1)
	guard (r < rn)

{-
makeSequence $ do
	selectSomething
	conditionSomethingSelected
	a <- utilityA
	b <- utilityb
	doSomething 
	return makeUtility (a+b) -- utility of this sequence
	-}