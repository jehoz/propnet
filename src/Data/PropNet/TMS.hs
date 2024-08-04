{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.PropNet.TMS where

import Control.Monad (foldM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable (hashWithSalt))
import Data.List (nubBy, sortOn)
import Data.PropNet.Partial

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

data Assumption = Assumption Name Value deriving (Eq, Show)

instance Hashable Assumption where
  hashWithSalt s (Assumption n v) = hashWithSalt s (n, v)

type Premise = HashSet Assumption

data TMS a = TMS
  { -- | A set of conditional beliefs about the current value:
    -- /"if some set of set of assumptions are true, then my value is x"/
    beliefs :: HashMap Premise a,
    -- | All of the premises that have been rejected for producing contradictions
    rejected :: HashSet Premise
  }
  deriving (Eq)

instance Functor TMS where
  fmap f (TMS blfs rej) = TMS (fmap f blfs) rej

instance (Eq a, Partial a) => Partial (TMS a) where
  bottom = TMS HashMap.empty HashSet.empty

  update t1 t2 = case updateBeliefs t1.beliefs t2.beliefs of
    Contradiction -> Contradiction
    Unchanged _ -> if t1.rejected /= rejected then Changed (TMS t1.beliefs rejected) else Unchanged t1
    Changed beliefs' -> Changed (TMS beliefs' rejected)
    where
      rejected = HashSet.union t1.rejected t2.rejected

      updateBeliefs old new = foldM integrateBelief old (HashMap.toList new)

      integrateBelief blfs (newPrem, newVal) =
        let res = sequenceA $ case HashMap.lookup newPrem blfs of
              Just oldVal -> HashMap.singleton newPrem (update oldVal newVal)
              Nothing -> HashMap.fromList $ (\(oldPrem, oldVal) -> (HashSet.union oldPrem newPrem, update oldVal newVal)) <$> HashMap.toList blfs
         in flip HashMap.union blfs <$> res

-- | A TMS with no information (no beliefs, no rejected premises)
empty :: TMS a
empty = TMS HashMap.empty HashSet.empty

-- | Create a TMS which takes the specified value as a given.
fromGiven :: a -> TMS a
fromGiven x = TMS (HashMap.singleton HashSet.empty x) HashSet.empty

-- | Add a belief to the TMS, overwriting any previous belief with the same
-- premise.
believe :: (Premise, a) -> TMS a -> TMS a
believe (prem, x) (TMS blfs rej) = TMS (HashMap.insert prem x blfs) rej

-- | Add a premise to the rejection set.
reject :: Premise -> TMS a -> TMS a
reject prem (TMS blfs rej) = TMS blfs (HashSet.insert prem rej)

-- | Takes in a new belief (some value and the premise it's dependent on) and
-- tries to reconcile it with the other beliefs in the `TMS`.
-- If any contradictions are found, those premises are stored in the rejected
-- set.
assimilate :: (Partial a) => (Premise, a) -> TMS a -> TMS a
assimilate (prem, newVal) tms = foldr handleResult tms results
  where
    results = case HashMap.lookup prem tms.beliefs of
      Just oldVal -> [(prem, update oldVal newVal)]
      Nothing -> (\(oldPrem, oldVal) -> (HashSet.union oldPrem prem, update oldVal newVal)) <$> HashMap.toList tms.beliefs

    handleResult (p, res) = case res of
      Changed x -> believe (p, x)
      Unchanged _ -> id
      Contradiction -> reject p

-- | Prune redundant premises from rejected set and remove any beliefs that
-- depent on any rejected premise.
reanalyze :: TMS a -> TMS a
reanalyze (TMS blfs rej) =
  let rej' = HashSet.fromList $ nubBy HashSet.isSubsetOf $ sortOn HashSet.size (HashSet.toList rej)
      blfs' = HashMap.filterWithKey (\prem _ -> not $ any (`HashSet.isSubsetOf` prem) rej') blfs
   in TMS blfs' rej
