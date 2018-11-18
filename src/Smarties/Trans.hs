{-|
Module      : Smarties.Trans
Description : Same as Smarties except with monad transformer.
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@gmail.com
Stability   : experimental

This is a drop in replacement of Smarties.Base with MTL support.
-}

module Smarties.Trans (
    module Smarties.Trans.Base,
    module Smarties.Trans.Nodes,
    module Smarties.Trans.Builders,
) where

import Smarties.Trans.Base
import Smarties.Trans.Nodes
import Smarties.Trans.Builders
