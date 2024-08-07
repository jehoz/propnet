{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.PropNet.TMS where

import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable (hashWithSalt))
import Data.List (nub, nubBy, sortOn)
import Data.PropNet.Partial
import Data.PropNet.Partial.EnumSet (EnumSet)
import qualified Data.PropNet.Partial.EnumSet as EnumSet

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
-- conjunction of some number of `Assumption`s which we store in a set.
-- The empty set, in this case, represents a given.
type Premise = HashSet Assumption

implies :: Premise -> Premise -> Bool
implies = flip HashSet.isSubsetOf

-- | A Truth Maintainance System simultaneously holds multiple (potentially
-- conflicting) conditional beliefs about a particular value.
--
-- Each belief is dependent on some `Premise`, and dictates what we know about
-- our value if that premise is true.  For example, if we use an `EnumSet` to
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
  fmap f (TMS blfs rej) = TMS (fmap f blfs) rej

instance (Eq a, Partial a) => Partial (TMS a) where
  bottom = TMS HashMap.empty HashSet.empty

  leq (TMS blfs1 rej1) (TMS blfs2 rej2) =
    blfs1 `HashMap.isSubmapOf` blfs2 && rej1 `HashSet.isSubsetOf` rej2

  update t1 t2
    -- rejecting the given premise (empty set) means there is no solution
    | HashSet.member HashSet.empty t3.rejected = Contradiction
    | t1 == t3 = Unchanged t1
    | otherwise = Changed t3
    where
      t3 = combine t1 t2

-- | A TMS with no information (no beliefs, no rejected premises)
empty :: TMS a
empty = TMS HashMap.empty HashSet.empty

-- | Create a TMS which takes the specified value as a given.
fromGiven :: a -> TMS a
fromGiven x = TMS (HashMap.singleton HashSet.empty x) HashSet.empty

-- | Get the believed value for a given premise (if it exists)
consequentOf :: Premise -> TMS a -> Maybe a
consequentOf prem (TMS blfs _) = HashMap.lookup prem blfs

-- | Get consequents of current beliefs which have the most information.
bestGuesses :: (Partial a) => TMS a -> [a]
bestGuesses (TMS blfs _) = maxima (HashMap.elems blfs)

-- | Get unique values across all best guesses
bestPossibilities :: (Eq a, Bounded a, Enum a) => TMS (EnumSet a) -> [a]
bestPossibilities = nub . concatMap EnumSet.toList . bestGuesses

-- | Add a belief to the TMS, overwriting any previous belief with the same
-- premise.
--
-- __NOTE__: This does not affect any of the other beliefs in the TMS or ensure
-- that the premise is valid. If that is what you want, use `assimilate`.
believe :: (Premise, a) -> TMS a -> TMS a
believe (prem, x) (TMS blfs rej) = TMS (HashMap.insert prem x blfs) rej

-- | Add a premise to the rejection set.
--
-- __NOTE__: This does not discard current beliefs that depend on the rejected
-- premise.  If you use this function directly, you probably want to `reanalyze`
-- the TMS afterwards.
reject :: Premise -> TMS a -> TMS a
reject prem (TMS blfs rej) = TMS blfs (HashSet.insert prem rej)

-- | Is the premise valid?
--
-- This checks two things:
-- 1. Is the premise non-contradictory (at most one assumption for each name)
-- 2. Ensure that the premise doesn't imply any of the ones we've rejected
isPlausible :: Premise -> TMS a -> Bool
isPlausible prem tms =
  let names = HashSet.fromList $ (\(Assumption name _) -> name) <$> HashSet.toList prem
   in (length names == length prem) && not (any (`HashSet.isSubsetOf` prem) tms.rejected)

-- | Prune redundant premises from rejected set and remove any beliefs that
-- depent on any rejected premise.
--
-- This is done for you when you `assimilate` or `combine`, so you probably
-- shouldn't need to use this function directly.
reanalyze :: TMS a -> TMS a
reanalyze (TMS blfs rej) =
  let rej' = HashSet.fromList $ nubBy HashSet.isSubsetOf $ sortOn HashSet.size (HashSet.toList rej)
      blfs' = HashMap.filterWithKey (\prem _ -> isPlausible prem (TMS blfs rej')) blfs
   in TMS blfs' rej'

-- | Takes in a new belief and logically combines it with the other beliefs in
-- the TMS.
-- If any contradictions are found, those premises are stored in the rejected
-- set.
assimilate :: (Partial a) => (Premise, a) -> TMS a -> TMS a
assimilate blf = reanalyze . assimilateInner blf

-- | Assimilate without reanalyzing
assimilateInner :: (Partial a) => (Premise, a) -> TMS a -> TMS a
assimilateInner (prem, newVal) tms =
  if isPlausible prem tms
    then foldr handleResult tms results
    else tms
  where
    results = case HashMap.lookup prem tms.beliefs of
      Just oldVal -> [(prem, update oldVal newVal)]
      Nothing ->
        HashMap.toList tms.beliefs
          <&> ( \(oldPrem, oldVal) ->
                  let combo = (HashSet.union oldPrem prem, update oldVal newVal)
                   in if oldVal == newVal
                        then
                          if
                            | oldPrem `implies` prem -> (prem, Changed newVal)
                            | prem `implies` oldPrem -> (oldPrem, Unchanged newVal)
                            | otherwise -> combo
                        else combo
              )

    handleResult (p, res) = case res of
      Changed x -> believe (p, x)
      Unchanged _ -> id
      Contradiction -> reject p

-- | Like `assimilate` but combines all of the beliefs in one TMS with all of
-- the beliefs in another, and takes the union of their two rejected sets.
combine :: (Partial a) => TMS a -> TMS a -> TMS a
combine t1 t2 =
  let rejected = HashSet.union t1.rejected t2.rejected
   in reanalyze $ foldr assimilateInner (TMS t1.beliefs rejected) (HashMap.toList t2.beliefs)
