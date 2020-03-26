{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}

-- NOTE these tests are almost identical to the ones in SmartiesSpec
-- I wish there was a nice way to reuse code here :\

module SmartiesTransSpec where

import           Prelude                hiding (sequence)

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad          (forM_, liftM, liftM2, replicateM)
import           Control.Monad.Identity (runIdentity)
import           Data.List              (findIndex, maximum)
import           Data.Maybe             (fromMaybe)

import           Smarties.Trans

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

-- | adds an action that increases state by 1 (used for tracking which node was executed)
addAction :: Int -> NodeSequence GeneratorType PerceptionType OutputType ()
addAction n = fromAction $ SimpleAction (\_ -> (+n))

prop_selector_basic :: Bool -> Bool
prop_selector_basic b = let
    tree = selector [result FAIL, result FAIL, result FAIL, result FAIL, result FAIL, result FAIL, result FAIL, result (if b then SUCCESS else FAIL)]
    (_,_,_,s,_) = runIdentity $ (runNodes tree) () ()
    in if b then s == SUCCESS else s == FAIL

-- prop_weightedSelector :: Bool
-- prop_weightedSelector = True

prop_utilitySelector :: [Int] -> Bool
prop_utilitySelector w = r where
    tree = utilitySelector $ map (\n-> addAction n >> return (w!!n)) [0..(length w -1)]
    (_,p,s,os) = execNodeSequence tree () 0
    rslt = reduce os p
    r = if length w == 0
      then s == FAIL
      else rslt == fromMaybe (-1) (findIndex (maximum w ==) w)

-- prop_utilityWeightedSelector :: Bool
-- prop_utilityWeightedSelector = True

prop_addition :: Int -> (NonNegative Int) -> Bool
prop_addition a (NonNegative b) = execNodeSequenceTimesFinalize b (addAction a) () 0 == a*b

prop_addition_sequence :: Int -> (NonNegative Int) -> Bool
prop_addition_sequence a (NonNegative b) = (reduce os p == a*b) && s == FAIL where
    (_,p,s,os) = execNodeSequence tree () 0
    tree = do
        forM_ [0..(b-1)] (\_->addAction a)
        -- execution should stop here
        result FAIL
        forM_ [0..100] (\_->addAction a)


-- TODO


-- Template haskell nonsense to run all properties prefixed with "prop_" in this file
return []
props = $allProperties

-- hspec nonsense
spec = forM_ props (\(s,p) -> it s $ property $ p)
