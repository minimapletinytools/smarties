{-|
Module      : Base
Description : MTL equivalent of Smarties.Base
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@gmail.com
Stability   : experimental
-}

module Smarties.Base (
  SelfActionable(..),
  reduce,
  Status(..),
  NodeSequenceT(..),
  execNodeSequenceT,
  execNodeSequenceTimesT,
  execNodeSequenceTimesFinalizeT,
  NodeSequence,
  runNodeSequence,
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

import Lens.Micro
import Control.Monad.Random
import Control.Monad.Identity (Identity, runIdentity)
import Control.Applicative




--https://ccrma.stanford.edu/~jos/sasp/Product_Two_Gaussian_PDFs.html
--https://en.wikipedia.org/wiki/Sum_of_normally_distributed_random_variables

class SelfActionable p o where
  apply :: o -> p -> p

-- probably {-# OVERLAPPABLE #-}
instance SelfActionable a (a->a) where
  apply = ($)

-- | reduce a list of actions and apply it the perception
reduce :: (SelfActionable p o) => [o] -> p -> p
reduce os p = foldr apply p os

data Status = SUCCESS | FAIL deriving (Eq, Show)

-- | behavior tree sequence monad type
-- `g` is rng type
-- `p` is perception type (state)
-- `o` is output type
-- behavior tree output list is ordered from right to left, i.e. use foldr when applying
-- this looks a lot like (StateT (g,p) Writer o) but behaves differently when nodes fail
newtype NodeSequenceT g p o m a =  NodeSequenceT { runNodeSequenceT :: g -> p -> m (a, g, p, Status, [o]) }

-- | run a node sequence and throw out its monadic output
execNodeSequenceT :: (Monad m) => NodeSequenceT g p o m a -> g -> p -> m (g, p, Status, [o])
execNodeSequenceT n g p = (runNodeSequenceT n) g p >>= (\(_,g',p',s,os) -> return (g',p',s,os))

-- internal helper
iterate_ :: (Monad m) => Int -> (a -> m a) -> a -> m a
iterate_ n f = foldr (>=>) return (replicate n f)

-- | run a node sequence several times using its output to generate the next perception state
execNodeSequenceTimesT :: (SelfActionable p o, Monad m) => Int -> NodeSequenceT g p o m a -> g -> p -> m (g, p, Status, [o])
execNodeSequenceTimesT num n _g _p = iterate_ num itfun (_g, _p, SUCCESS, []) where
  itfun (g,p,_,os) = execNodeSequenceT n g (reduce os p)

-- | same as 'runNodeSequenceTequenceTimes' except reduces the final input with its output and only returns this result
execNodeSequenceTimesFinalizeT :: (SelfActionable p o, Monad m) => Int -> NodeSequenceT g p o m a -> g -> p -> m p
execNodeSequenceTimesFinalizeT num n _g _p = do
  (_,p,_,os) <- execNodeSequenceTimesT num n _g _p
  return $ reduce os p

-- $nontransformerlink
type NodeSequence g p o a = NodeSequenceT g p o Identity a

-- |
runNodeSequence :: NodeSequence g p o a -> g -> p -> (a, g, p, Status, [o])
runNodeSequence n g p = runIdentity $ runNodeSequenceT n g p

-- |
execNodeSequence :: NodeSequence g p o a -> g -> p -> (g, p, Status, [o])
execNodeSequence n g p = runIdentity $ execNodeSequenceT n g p

-- |
execNodeSequenceTimes :: (SelfActionable p o) => Int -> NodeSequence g p o a -> g -> p -> (g, p, Status, [o])
execNodeSequenceTimes num n g p = runIdentity $ execNodeSequenceTimesT num n g p

-- |
execNodeSequenceTimesFinalize :: (SelfActionable p o) => Int -> NodeSequence g p o a -> g -> p -> p
execNodeSequenceTimesFinalize num n g p = runIdentity $ execNodeSequenceTimesFinalizeT num n g p

-- $helperlink

-- | returns the perception
getPerception :: (Monad m) => NodeSequenceT g p o m p
getPerception = NodeSequenceT $ (\g p -> return (p, g, p, SUCCESS, []))

-- | sets the perception
setPerception :: (Monad m) => p -> NodeSequenceT g p o m ()
setPerception p' = NodeSequenceT $ (\g _ -> return ((), g, p', SUCCESS, []))

-- | add an item to the output
tellOutput :: (Monad m) => o -> NodeSequenceT g p o m ()
tellOutput o = NodeSequenceT $ (\g p -> return ((), g, p, SUCCESS, [o]))

-- | returns the rng
getGenerator :: (Monad m) => NodeSequenceT g p o m g
getGenerator = NodeSequenceT $ (\g p -> return (g, g, p, SUCCESS, []))

-- | sets the rng
setGenerator :: (Monad m) => g -> NodeSequenceT g p o m ()
setGenerator g = NodeSequenceT $ (\_ p -> return ((), g, p, SUCCESS, []))


instance (Functor m, Monad m) => Functor (NodeSequenceT g p o m) where
  fmap :: (a -> b) -> NodeSequenceT g p o m a -> NodeSequenceT g p o m b
  fmap f n = do
    a <- n
    return $ f a
  --fmap f n = NodeSequenceT func where
  --    func g_ p_ = fmap f' ((runNodeSequenceT n) g_ p_) where
  --        f' (a, g, p, s, os) = (f a, g, p, s, os)

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

instance (Monad m) => Monad (NodeSequenceT g p o m) where
  (>>=) :: NodeSequenceT g p o m a -> (a -> NodeSequenceT g p o m b) -> NodeSequenceT g p o m b
  NodeSequenceT n >>= f = NodeSequenceT func where
    func g p = do
      -- evaluate the node
      (a, g', p', s, os) <- n g p
      let
        NodeSequenceT n' = f a -- generate the next node
      rslt <- (n' g' p') -- run the next node
      let
        keepGoing = over _5 (++os) rslt
        (b,g'',_,_,_) = keepGoing
      if s == FAIL
       -- if the current node is FAIL:
        -- status is FAIL
        -- perception is input perception
        -- output is empty
        -- rng is accumulated rng from next monad
        -- return monadic return value by dry executing the next monad (passing through updated perception and tossing results)
        -- N.B. if your internal monad encodes side effects, they will not be reverted!
       then return (b, g'', p, FAIL, [])
       else return keepGoing where

instance MonadTrans (NodeSequenceT g p o) where
  lift m = NodeSequenceT (\g p -> m >>= (\a -> return (a, g, p, SUCCESS,[])))

instance (RandomGen g, Monad m) => MonadRandom (NodeSequenceT g p o m) where
  getRandoms = forM [0..] (const getRandom)
  getRandomRs r = forM [0..] (const $ getRandomR r)
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
