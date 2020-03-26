{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module BuildersSpec where

import           Prelude                hiding (sequence)

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad          (forM_, liftM, liftM2, replicateM)
import           Control.Monad.Identity (runIdentity)
import           Data.List              (findIndex, maximum)
import           Data.Maybe             (fromMaybe)

import           Smarties.Trans


test_fromUtility_basic :: Expectation
test_fromUtility_basic = status `shouldBe` SUCCESS where
  util n = fromUtility $ Utility (\g _ -> (n, g))
  tree = do
    a <- util 5
    result $ if a == 5 then SUCCESS else FAIL
  (_, _, status, _) = execNodeSequence tree () ()


test_fromUtility_withUtilitySelector :: Expectation
test_fromUtility_withUtilitySelector = status `shouldBe` FAIL where
  util n = fromUtility $ Utility (\g _ -> (n, g))
  tree = do
    utilitySelector $ [
        (result SUCCESS >> util (1 :: Int))
        , (result FAIL >> util 2)
        , (result SUCCESS >> util 0)
      ]
  (_, _, status, _) = execNodeSequence tree () ()


test_fromPerception_basic :: Expectation
test_fromPerception_basic = (p, status) `shouldBe` (3, SUCCESS) where
  tree1 = fromPerception $ SimplePerception (\p -> (p+1))
  tree2 = fromPerception $ Perception (\g p -> (g, p+2))
  (_, p, status, _) = execNodeSequence (tree1 >> tree2) () 0


test_fromCondition_basic :: Expectation
test_fromCondition_basic = (status1, status2) `shouldBe` (SUCCESS, FAIL) where
  cond1 = fromCondition $ SimpleCondition (\_ -> True)
  cond2 = fromCondition $ Condition (\g _-> (False, g))
  (_, _, status1, _) = execNodeSequence cond1 () ()
  (_, _, status2, _) = execNodeSequence cond2 () ()

test_fromAction_basic :: Expectation
test_fromAction_basic = (status, os) `shouldBe` (SUCCESS, reverse vals) where
  action n = fromAction $ SimpleAction (\_ -> n)
  vals = [0..100]
  tree = forM_ vals action
  (_, _, status, os) = execNodeSequence tree () ()

--test_fromSelfAction_basic :: Expectation
--test_fromSelfAction_basic = r where
--  action n = fromSelfAction $ SelfAction (\g p -> (g, (+n)))


spec :: Spec
spec = do
  describe "Builders" $ do
    describe "Utility" $ do
      it "passes basic tests" $
        test_fromUtility_basic
      it "works as expected in utilitySelector" $
        test_fromUtility_withUtilitySelector
    describe "Perception" $ do
      it "passes basic tests" $
        test_fromPerception_basic
    describe "Condition" $ do
      it "passes basic tests" $
        test_fromCondition_basic
    describe "Action" $ do
      it "passes basic tests" $
        test_fromAction_basic
