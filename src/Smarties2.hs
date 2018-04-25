module Smarties2 (
    module Smarties2.Base,
    module Smarties2.Nodes,
    module Smarties2.Builders,
    module Smarties2.TreeState
) where

import Smarties2.Base
import Smarties2.TreeState
import Smarties2.Nodes
import Smarties2.Builders


-- TODO 2.0
-- switch over from type class to data type
-- add type safety to number of children and node type if possible
-- think of a way to allow output type of a node monad be used to compute utility
-- separate random number generator from perception so we can update the former without updating the latter
-- create helper function for creating utility distribution from just a float
-- add lenses and other helper functions for pattern matching on Utility becaues it's a huge pain right now..