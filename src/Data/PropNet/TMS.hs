module Data.PropNet.TMS where

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)

-- Truth Maintainance System
-- A TMS is a set of conditional beliefs about its own state:
--   if x=1 then (a, b, c, d, or e)
--   if x=1 & y=2 then (a, b, or c)
-- The premise refers to some cell in the network and an assumed value for that
-- cell.

-- The beginning state of the network and deductions from it are dependent on Given
-- When a branch point is reached, an Assumption premise is made for some Cell
-- (picked for least entropy) for each possible value of Cell.  Then we do something like
-- tryWith (Assumption C3 7)
-- the assumption is added to the set of premises by applying the value 7 to C3
-- belief with that assumption as dependency.
-- then downstream cells update their value with that dependency
-- if it fails, add it to set of rejected premises

type Name = Int

type Value = Int

data Premise
  = Given
  | Assumption Name Value

data TMS a = TMS
  { -- | A set of conditional beliefs about the current value
    -- /"if some set of premises are true, then my value is x"/
    beliefs :: HashMap (HashSet Premise) a,
    -- | All of the combinations of premises that have been rejected.
    badPremises :: HashSet Premise
  }
