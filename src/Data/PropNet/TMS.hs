{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.PropNet.TMS where

import Control.Monad (guard)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable (hashWithSalt))
import Data.List (maximumBy)
import Data.PropNet.Partial

-- | A unique identifier for a parameter in our system (ie. some `Cell` in the
-- propagator network).
type Name = Int

-- | An integer encoding a defined value for some cell.
type Value = Int

-- | The assumption that some cell has some defined value.
--
-- An `Assumption` is primarily a way of representing a branch in a search tree.
-- The exact way of assigning `Name`s to cells and encoding their values as
-- `Value`s shouldn't matter as long as both mappings are injective.
data Assumption = Assumption Name Value deriving (Eq, Show)

instance Hashable Assumption where
  hashWithSalt s (Assumption n v) = hashWithSalt s (n, v)

-- | The antecedent of some belief.  For our purposes this will always be a
-- conjunctive clause joining some number of `Assumption`s or their negation.
-- An empty map, in this case, represents a given.
type Premise = HashMap Assumption Bool

-- | Does the first premise subsume the second?
-- (Are all of the assumptions in the second contained within the first?)
subsumes :: Premise -> Premise -> Bool
subsumes = flip HashMap.isSubmapOf

addAssumption :: Assumption -> Bool -> Premise -> Premise
addAssumption = HashMap.insert

negateAssumption :: Assumption -> Premise -> Premise
negateAssumption = HashMap.update (Just . not)

removeAssumption :: Assumption -> Premise -> Premise
removeAssumption = HashMap.delete

-- | A Truth Maintainance System simultaneously holds multiple (potentially
-- conflicting) conditional beliefs about a particular value.
--
-- Each belief is dependent on some `Premise`, and dictates what we know about
-- our value if that premise is true.  For example, if we use an `OneOf` to
-- represent a set of finite possibilities for our value, then our beliefs would
-- look like this:
--
-- * if \(x=1\) then my value is in the set \(\{ a, b, c, d, e \}\)
-- * if \(x=1\) and \(y=2\) then my value is in the set \(\{ a, b, d \}\)
-- * etc.
--
-- We maintain multiple beliefs simultaneously because we will almost certainly
-- discover that some of the premises result in contradictions as we propagate
-- information throughout our system.  When this happens, the TMS rejects the
-- premise, discarding any beliefs which depended on it, and remembers the
-- rejected premise so that no future beliefs can be established which depend on
-- it.  This lets our system collectively "learn" to avoid large chunks of the
-- search space which will invariably lead to contradictions.
data TMS a = TMS
  { -- | A set of conditional beliefs about the current value.
    beliefs :: HashMap Premise a,
    -- | All of the premises that have been rejected for producing contradictions
    rejected :: HashSet Premise
  }
  deriving (Eq, Show)

instance Functor TMS where
  fmap f tms = tms {beliefs = fmap f tms.beliefs}

instance (Eq a, Partial a) => Partial (TMS a) where
  bottom = empty

  leq (TMS blfs1 rej1) (TMS blfs2 rej2) =
    blfs1 `HashMap.isSubmapOf` blfs2 && rej1 `HashSet.isSubsetOf` rej2

  update t1 t2
    -- rejecting the given premise (empty set) means there is no solution
    | HashSet.member HashMap.empty t3.rejected = Contradiction
    | t1 == t3 = Unchanged t1
    | otherwise = Changed t3
    where
      t3 = combine t1 t2

-- | A TMS with no information (no beliefs, no rejected premises)
empty :: TMS a
empty = TMS HashMap.empty HashSet.empty

-- | Create a TMS which takes the specified value as a given.
fromGiven :: a -> TMS a
fromGiven x = TMS (HashMap.singleton HashMap.empty x) HashSet.empty

-- | Get the belief representing the deepest non-rejected branch we've visited
-- in the search tree.  (i.e. the premise which contains the most assumptions)
deepestBranch :: TMS a -> (Premise, a)
deepestBranch tms = maximumBy (compare `on` (HashMap.size . fst)) (HashMap.toList tms.beliefs)

-- | Get the believed value for a given premise (if it exists)
consequentOf :: Premise -> TMS a -> Maybe a
consequentOf prem tms = HashMap.lookup prem tms.beliefs

-- | Add a belief to the TMS, overwriting any previous belief with the same
-- premise.
--
-- __NOTE__: This does not affect any of the other beliefs in the TMS or ensure
-- that the premise is valid. If that is what you want, use `assimilate`.
believe :: (Premise, a) -> TMS a -> TMS a
believe (prem, x) tms = tms {beliefs = HashMap.insert prem x tms.beliefs}

-- | Reject a premise, asserting that we will always encounter a contradiction
-- if we take it to be true.
--
-- Does some additional processing to generalize invalid premises as well
-- (if @(A && B)@ and @(A && not B)@ both rejected, then we can reject @A@).
--
-- __NOTE__: This does not discard current beliefs that depend on the rejected
-- premise.  If you use this function directly, you probably want to `prune`
-- the TMS afterwards.
reject :: Premise -> TMS a -> TMS a
reject prem tms =
  let induced = do
        a <- HashMap.keys prem
        let inverse = negateAssumption a prem
        guard (not (valid inverse tms))
        let parent = removeAssumption a prem
        pure (reject parent . deleteRej inverse)

      insertRej p t = t {rejected = HashSet.insert p t.rejected}
      deleteRej p t = t {rejected = HashSet.delete p t.rejected}
   in case induced of
        [] -> insertRej prem tms
        fs -> foldr ($) tms fs

-- | Is the premise valid within our TMS?
-- (Check that is is not subsumed by any of the premises we've rejected).
valid :: Premise -> TMS a -> Bool
valid prem tms = not (any (prem `subsumes`) tms.rejected)

-- | Remove any beliefs that are subsumed by a rejected premise.
prune :: TMS a -> TMS a
prune tms =
  let blfs' = HashMap.filterWithKey (\p _ -> valid p tms) tms.beliefs
   in tms {beliefs = blfs'}

-- | Takes in a belief and incorporates it into the other beliefs in the TMS,
-- resolving any contradictions that are found in the process.
assimilate :: (Partial a) => (Premise, a) -> TMS a -> TMS a
assimilate (prem, newVal) tms
  | valid prem tms = change tms
  | otherwise = tms
  where
    change = case HashMap.lookup prem tms.beliefs of
      Just oldVal -> case update oldVal newVal of
        Unchanged _ -> id
        Changed x -> believe (prem, x)
        Contradiction -> prune . reject prem
      Nothing ->
        let closest = head $ maxima $ [v | (p, v) <- HashMap.toList tms.beliefs, prem `subsumes` p]
         in case update closest newVal of
              Unchanged x -> believe (prem, x)
              Changed x -> believe (prem, x)
              Contradiction -> prune . reject prem

-- | Like `assimilate` but combines all of the beliefs in one TMS with all of
-- the beliefs in another, and takes the union of their two rejected sets.
combine :: (Partial a) => TMS a -> TMS a -> TMS a
combine t1 t2 =
  let rejected = HashSet.union t1.rejected t2.rejected
      t1' = if rejected /= t1.rejected then prune (t1 {rejected = rejected}) else t1
   in foldr assimilate t1' (HashMap.toList t2.beliefs)
