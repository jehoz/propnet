{-# LANGUAGE OverloadedRecordDot #-}

module Data.PropNet.Relation where

import Data.HashMap.Strict (keysSet)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Maybe (fromMaybe)
import Data.PropNet.Partial (Partial, bottom)
import Data.PropNet.TMS (TMS (..), believe)

type BinaryR a b = ((a, b) -> (a, b))

type TernaryR a b c = ((a, b, c) -> (a, b, c))

-- | Lift a binary relation over two types into a relation over two `TMS`'s of
-- those types.
liftTms2 :: (Partial a, Partial b) => BinaryR a b -> BinaryR (TMS a) (TMS b)
liftTms2 r (t1, t2) = foldr iter (initTms, initTms) prems -- should we reanalyze the results?
  where
    initTms = TMS HashMap.empty (HashSet.union t1.rejected t2.rejected)

    prems = HashSet.union (keysSet t1.beliefs) (keysSet t2.beliefs)

    iter p (t1', t2') =
      let x = fromMaybe bottom (HashMap.lookup p t1.beliefs)
          y = fromMaybe bottom (HashMap.lookup p t2.beliefs)
          (x', y') = r (x, y)
       in (believe (p, x') t1', believe (p, y') t2')

-- | Lift a ternary relation over three types into a relation over three
-- `TMS`'s of those types.
liftTms3 :: (Partial a, Partial b, Partial c) => ((a, b, c) -> (a, b, c)) -> (TMS a, TMS b, TMS c) -> (TMS a, TMS b, TMS c)
liftTms3 r (t1, t2, t3) = foldr iter (initTms, initTms, initTms) prems
  where
    initTms = TMS HashMap.empty (HashSet.unions [t1.rejected, t2.rejected, t3.rejected])

    prems = HashSet.unions [keysSet t1.beliefs, keysSet t2.beliefs, keysSet t3.beliefs]

    iter p (t1', t2', t3') =
      let x = fromMaybe bottom (HashMap.lookup p t1.beliefs)
          y = fromMaybe bottom (HashMap.lookup p t2.beliefs)
          z = fromMaybe bottom (HashMap.lookup p t3.beliefs)
          (x', y', z') = r (x, y, z)
       in (believe (p, x') t1', believe (p, y') t2', believe (p, z') t3')
