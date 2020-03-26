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


spec :: Spec
spec = do
  describe "Builders" $ do
    describe "Utility" $ do
      it "passes basic test" $
        test_fromUtility_basic
      it "works as expected in utilitySelector" $
        test_fromUtility_withUtilitySelector
