{-# LANGUAGE TemplateHaskell #-}

module SmartiesSpec where




import Prelude hiding (sequence)

import Test.QuickCheck
import Test.Hspec

import Data.List (maximum, findIndex)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM, liftM2, replicateM, forM_)

import Smarties

data BranchType = BrSelector | BrSequence | BrNot deriving (Show)

instance Arbitrary BranchType where 
    arbitrary = oneof (fmap return [BrSelector, BrSequence])

data Tree = Leaf Bool | Branch BranchType [Tree] deriving (Show)

-- TODO find better formula for number of branches/children
instance Arbitrary Tree where
    arbitrary = sized tree'
        where 
            tree' 0 = liftM Leaf arbitrary
            tree' n = 
                frequency [(1, liftM Leaf arbitrary), 
                    (1, liftM2 Branch (return BrNot) subtree2), 
                    (3, liftM2 Branch arbitrary subtree)]
                    where 
                        subtree = replicateM n $ tree' (n `div` 2)
                        subtree2 = replicateM 1 $ tree' (n `div` 2)


-- for these examples, the output is designed to operate on the perception
type GeneratorType = ()
type PerceptionType = Int 
type OutputType = Int -> Int

instance TreeState PerceptionType

addAction :: Int -> NodeSequence GeneratorType PerceptionType OutputType ()
addAction n = fromAction $ SimpleAction (\_ -> (+n))

prop_selector_basic :: Bool -> Bool
prop_selector_basic b = let
	tree = selector [result FAIL, result FAIL, result FAIL, result FAIL, result FAIL, result FAIL, result FAIL, result (if b then SUCCESS else FAIL)]
	(_,_,_,s,_) = (runNodes tree) () () 
	in if b then s == SUCCESS else s == FAIL

-- prop_weightedSelector :: Bool
-- prop_weightedSelector = True

prop_utilitySelector :: [Int] -> Bool
prop_utilitySelector w = emptyCase || otherCase where
	tree = utilitySelector $ map (\n-> addAction n >> return (w!!n)) [0..(length w -1)]
	(_,p,s,os) = runNodeSequence tree () 0
	rslt = reduce os p
	emptyCase = if length w == 0 then s == FAIL else True
	otherCase = rslt == fromMaybe (-1) (findIndex (maximum w ==) w)

-- prop_utilityWeightedSelector :: Bool
-- prop_utilityWeightedSelector = True

prop_addition :: Int -> (NonNegative Int) -> Bool
prop_addition a (NonNegative b) = runNodeSequenceTimesFinalize b (addAction a) () 0 == a*b

prop_addition_sequence :: Int -> (NonNegative Int) -> Bool
prop_addition_sequence a (NonNegative b) = (reduce os p == a*b) && s == SUCCESS where
	tree = forM_ [0..(b-1)] (\_->addAction a)
	(_,p,s,os) = runNodeSequence tree () 0

-- TODO


--Template haskell nonsense to run all properties prefixed with "prop_" in this file 
return []
props = $allProperties

spec = forM_ props (\(s,p) -> it s $ property $ p)