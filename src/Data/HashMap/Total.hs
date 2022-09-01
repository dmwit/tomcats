{-# Language DeriveFunctor #-}
{-# Language DerivingVia #-}

module Data.HashMap.Total (
	TMap,
	-- * Total map terminology
	fromPartial, empty, insert, singleton, (!),
	-- * Function terminology
	tmap, const, set, indicator, ($),
	-- * Miscellaneous
	tabulate, trim
	) where

import Prelude hiding (const, ($))
import qualified Prelude as P

import Control.Applicative hiding (empty)
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Monoid

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

-- | Finitely represented functions/total maps. Represented as a partial
-- map and a default value. @TMap a b@ instances match the behavior of @a
-- -> b@ instances, except the 'Show' instance, which should be avoided except
-- for debugging, as it reveals implementation details that users are not
-- supposed to be able to know.
data TMap a b = TMap b (HashMap a b)
	deriving (Functor, Show)
	deriving (Semigroup, Monoid, Num) via Ap (TMap a) b

instance Hashable a => Applicative (TMap a) where
	pure = const
	f@(TMap defF mF) <*> x@(TMap defX mX) = tabulate
		(defF defX)
		(HM.keysSet mF <> HM.keysSet mX)
		(\a -> (f $ a) (x $ a))

tmapJoin :: Hashable a => TMap a (TMap a b) -> TMap a b
tmapJoin (TMap defT@(TMap defB mB) mT) = TMap defB (HM.mapWithKey (flip ($)) mT `HM.union` mB)

instance Hashable a => Monad (TMap a) where
	m >>= f = tmapJoin (fmap f m)

-- I wonder why Ap doesn't have this instance
instance (Hashable a, Fractional b) => Fractional (TMap a b) where
	(/) = liftA2 (/)
	recip = liftA recip
	fromRational = pure . fromRational

fromPartial :: v -> HashMap k v -> TMap k v
tmap :: b -> HashMap a b -> TMap a b
fromPartial = tmap
tmap = TMap

empty :: v -> TMap k v
const :: b -> TMap a b
empty = const
const b = TMap b HM.empty

insert :: Hashable k => k -> v -> TMap k v -> TMap k v
set :: Hashable a => a -> b -> TMap a b -> TMap a b
insert = set
set a b (TMap def m) = TMap def (HM.insert a b m)

singleton :: Hashable k => k -> v -> v -> TMap k v
indicator :: Hashable a => a -> b -> b -> TMap a b
singleton = indicator
indicator a eq ineq = TMap ineq (HM.singleton a eq)

(!) :: Hashable k => TMap k v -> k -> v
($) :: Hashable a => TMap a b -> a -> b
(!) = ($)
TMap def m $ a = case HM.lookup a m of
	Just b -> b
	Nothing -> def

tabulate :: v -> HashSet k -> (k -> v) -> TMap k v
tabulate def s f = TMap def (HM.mapWithKey (\k _ -> f k) . HS.toMap P.$ s)

trim :: Eq v => TMap k v -> TMap k v
trim (TMap def m) = TMap def (HM.filter (def/=) m)

-- TODO: could support a range operation and maybe some instances like Eq and
-- Ord or something, if we had a class for telling the cardinality of domains,
-- e.g.
--
-- class Card a where card :: Int
-- range :: (Card a, Hashable b) => TMap a b -> HashSet b
