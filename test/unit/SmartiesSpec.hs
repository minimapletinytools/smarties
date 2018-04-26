{-# LANGUAGE TemplateHaskell #-}

module SmartiesSpec where




import Prelude hiding (sequence)

import Test.QuickCheck
import Test.Hspec

import Control.Monad (forM_)

import Smarties


prop_dummy :: Bool
prop_dummy = True

-- TODO


--Template haskell nonsense to run all properties prefixed with "prop_" in this file 
return []
props = $allProperties

spec = forM_ props (\(s,p) -> it s $ property $ p)