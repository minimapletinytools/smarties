{-# LANGUAGE TemplateHaskell #-}

module NotSoSmartiesSpec where

import Test.Hspec

import Prelude hiding (sequence)

import NotSoSmarties hiding (Result)
import qualified NotSoSmarties as Smarties

import Test.QuickCheck
import Test.QuickCheck.All (allProperties)
import Control.Monad (liftM, liftM2, replicateM, forM_)

import           Control.Exception.Base   (assert)


-- TODO add this to tree
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

{-
instance CoArbitrary BranchType where
    coarbitrary _ = \_ -> arbitrary

-- probably am not using this..
instance CoArbitrary Tree where
    coarbitrary (Leaf n) = 
        variant 0 . coarbitrary n
    -- I feel like this needs to make use of t...
    coarbitrary (Branch t b) = 
        variant 1 . foldr (.) id (fmap coarbitrary b) 
-}

checkTree :: Tree -> Bool
checkTree (Leaf x) = x
checkTree (Branch b xs) = case b of
    BrSelector -> any checkTree xs
    BrSequence -> all checkTree xs
    BrNot -> assert (length xs == 1) . not . checkTree . head $ xs

basicSuccess :: WrapSmCondition Smarties.Result
basicSuccess = WrapSmCondition $ Smarties.Result SUCCESS

basicFail :: WrapSmCondition Smarties.Result 
basicFail = WrapSmCondition $ Smarties.Result FAIL

prop_basicHardcodedTest1 :: Bool
prop_basicHardcodedTest1 = 
    let 
        testTree :: SmTreeBuilder () () ()
        testTree = do
            con Selector $ do
                dec Not $ do
                    con Sequence $ do
                        con Selector $ do   
                            add basicSuccess
                con Sequence $ do
                    add basicSuccess
                    add basicSuccess
                    add basicSuccess
                    add basicSuccess
                    add basicFail
        (rslt, _, _) = tickTree (getTree testTree) ()
    in 
        rslt == FAIL

prop_basicHardcodedTest2 :: Bool
prop_basicHardcodedTest2 = 
    let 
        testTree :: SmTreeBuilder () () ()
        testTree = do
            utilitySelector $ do
                sequence $ do
                    utilityConst 0.1
                    rFail
                sequence $ do
                    utilityConst 0.1
                    rFail
                sequence $ do
                    utilityConst 0.1
                    rFail
                sequence $ do
                    utilityConst 0.1
                    rFail
                sequence $ do
                    utilityConst 0.2
                    rSuccess
        (rslt, _, _) = tickTree (getTree testTree) ()
    in 
        rslt == SUCCESS

prop_autoTest :: Tree -> Bool
prop_autoTest t = (rslt == SUCCESS) == checkTree t where
    testTree = getTree (buildTree t)
    rslt = (\(x,_,_) -> x) $ tickTree testTree ()
    buildTree :: Tree -> SmTreeBuilder () () ()
    buildTree (Leaf x) = if x then add basicSuccess else add basicFail
    buildTree (Branch b xs) = case b of
        BrSelector -> con Selector $ forM_ xs buildTree
        BrSequence -> con Sequence $ forM_ xs buildTree
        BrNot -> assert (length xs == 1) . dec Not . buildTree . head $ xs

--Template haskell nonsense to run all properties prefixed with "prop_" in this file 
return []
props = $allProperties

spec = forM_ props (\(s,p) -> it s $ property $ p)