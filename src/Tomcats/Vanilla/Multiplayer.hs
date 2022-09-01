module Tomcats.Vanilla.Multiplayer (
	-- * Top level
	parameters, parametersIO,
	T.mcts, T.initialize, T.descend, T.Tree(..),
	-- * Statistics and moves
	Moves(..),
	Statistics(..),
	won, lost, drawn, it'sComplicated,
	meanValuation,
	-- * Other
	ucb1,
	T.Parameters(..),
	) where

import Control.Applicative
import Data.Functor
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet, toMap)
import Data.HashMap.Total (TMap)
import System.Random.Stateful
import System.Random.MWC (createSystemRandom)

import qualified Data.HashMap.Total as TMap
import qualified Tomcats as T

parameters :: (Hashable move, Hashable player, StatefulGen g m) =>
	-- | 'T.clone'
	(position -> m position) ->
	-- | List the legal moves if the game is unfinished, or give an evaluation
	-- if it is finished. This function should not mutate the @position@ it is
	-- given.
	(position -> m (Moves move player)) ->
	-- | Tell which player made a move (so we know which valuation to optimize).
	(move -> player) ->
	-- | 'T.play'; implementers may assume that the position given is one that
	-- produced 'Finished' via the previous argument.
	(position -> move -> m ()) ->
	g -> T.Parameters m Double (Statistics player) move position
parameters clone moves player play g = T.rollout (T.uniform g) T.Parameters
	{ T.score = ucb1 . player
	, T.expand = fmap fromMoves . moves
	, T.clone = clone
	, T.play = play
	, T.preprocess = T.emptyPreprocessor
	}

-- | See 'parameters' for a breakdown of the arguments. This function is just
-- like that one, but it initializes an RNG on your behalf.
parametersIO :: (Hashable move, Hashable player) =>
	(position -> IO position) ->
	(position -> IO (Moves move player)) ->
	(move -> player) ->
	(position -> move -> IO ()) ->
	IO (T.Parameters IO Double (Statistics player) move position)
parametersIO clone moves player play = parameters clone moves player play <$> createSystemRandom

data Statistics player = Statistics
	{ visitCount :: {-# UNPACK #-} !Double
	, cumulativeValuations :: TMap player Double
	} deriving Show

-- | @won p@ is a suitable statistic for a game that's finished and won by @p@.
won :: Hashable player => player -> Statistics player
won p = Statistics 1 $ TMap.singleton p 1 0

-- | @lost p@ is a suitable statistic for a game that's finished and won by
-- everybody but @p@.
lost :: Hashable player => player -> Statistics player
lost p = Statistics 1 $ TMap.singleton p 0 1

-- | @drawn@ is a suitable statistic for a game that's finished, but nobody
-- really won or lost.
drawn :: Hashable player => Statistics player
drawn = Statistics 1 0.5

-- | In more complicated situations, each player can have arbitrary-ish
-- preferences. For each player, @1@ should be the most desirable outcome and
-- @0@ the least desirable.
it'sComplicated :: TMap player Double -> Statistics player
it'sComplicated = Statistics 1

meanValuation :: Statistics player -> TMap player Double
meanValuation stats = cumulativeValuations stats <&> (/visitCount stats)

instance Hashable player => Semigroup (Statistics player) where
	Statistics c v <> Statistics c' v' = Statistics (c+c') (v+v')

instance Hashable player => Monoid (Statistics player) where mempty = Statistics 0 0

-- | Suitable for use as a 'T.score'; compose with a function of type @move ->
-- player@. This is not needed if you plan to simply use the default behaviors
-- provided by 'parameters' or 'parametersIO'.
ucb1 :: Hashable player => player -> Statistics player -> Statistics player -> Double
ucb1 p parent child = T.ucb1 (visitCount parent) (visitCount child) (cumulativeValuations child TMap.! p)

data Moves move player = Finished (Statistics player) | Next (HashSet move) deriving Show

fromMoves :: (Hashable move, Hashable player) => Moves move player -> (Statistics player, HashMap move (Statistics player))
fromMoves (Finished stats) = (stats, mempty)
fromMoves (Next ms) = (mempty, mempty <$ toMap ms)
