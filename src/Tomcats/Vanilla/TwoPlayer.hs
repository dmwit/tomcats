module Tomcats.Vanilla.TwoPlayer (
	-- * Top level
	parameters, parametersIO,
	T.mcts, T.initialize, T.descend, T.Tree(..),
	-- * Statistics and moves
	Moves(..),
	Statistics(..),
	won, lost, drawn,
	meanValuation,
	-- * Other
	Player(..), otherPlayer,
	ucb1,
	T.Parameters(..),
	) where

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet, toMap)
import System.Random.Stateful
import System.Random.MWC (createSystemRandom)
import qualified Tomcats as T

parameters :: (Hashable move, StatefulGen g m) =>
	-- | 'T.clone'
	(position -> m position) ->
	-- | List the legal moves if the game is unfinished, or give an evaluation
	-- if it is finished. This function should not mutate the @position@ it is
	-- given.
	(position -> m (Moves move)) ->
	-- | Tell which player made a move (so we know whether to strive for low
	-- valuations or high valuations).
	(move -> Player) ->
	-- | 'T.play'; implementers may assume that the position given is one that
	-- produced 'Finished' via the previous argument.
	(position -> move -> m ()) ->
	g -> T.Parameters m Double Statistics move position
parameters clone moves player play g = T.rollout (T.uniform g) T.Parameters
	{ T.score = ucb1 . player
	, T.expand = fmap fromMoves . moves
	, T.clone = clone
	, T.play = play
	, T.preprocess = T.emptyPreprocessor
	}

-- | See 'parameters' for a breakdown of the arguments. This function is just
-- like that one, but it initializes an RNG on your behalf.
parametersIO :: Hashable move =>
	(position -> IO position) ->
	(position -> IO (Moves move)) ->
	(move -> Player) ->
	(position -> move -> IO ()) ->
	IO (T.Parameters IO Double Statistics move position)
parametersIO clone moves player play = parameters clone moves player play <$> createSystemRandom

-- | An explicit representation of the collection of players. The 'O' player
-- should like valuations of @0@, and the 'I' player should like valuations of
-- @1@. (Cute, right?)
data Player = O | I deriving (Eq, Ord, Read, Show, Bounded, Enum)

otherPlayer :: Player -> Player
otherPlayer O = I
otherPlayer I = O

data Statistics = Statistics
	{ visitCount, cumulativeValuation :: {-# UNPACK #-} !Double
	} deriving (Eq, Ord, Read, Show)

-- | @won p@ is a suitable statistic for a game that's finished and won by
-- @p@.
won :: Player -> Statistics
won p = Statistics 1 $ case p of
	O -> 0
	I -> 1

-- | @lost p@ is a suitable statistic for a game that's finished and lost by @p@. This is equivalent to combining 'won' and 'otherPlayer'.
lost :: Player -> Statistics
lost = won . otherPlayer

-- | A suitable statistic for a game that's finished and neither player won.
drawn :: Statistics
drawn = Statistics 1 0.5

meanValuation :: Statistics -> Double
meanValuation stats = cumulativeValuation stats / visitCount stats

instance Semigroup Statistics where
	Statistics c v <> Statistics c' v' = Statistics (c+c') (v+v')

instance Monoid Statistics where mempty = Statistics 0 0

-- | Suitable for use as a 'T.score'; compose with a function of type @move ->
-- Player@. This is not needed if you plan to simply use the default behaviors
-- provided by 'parameters' or 'parametersIO'.
ucb1 :: Player -> Statistics -> Statistics -> Double
ucb1 p parent child = T.ucb1 (visitCount parent) (visitCount child) $ case p of
	O -> visitCount child - cumulativeValuation child
	I -> cumulativeValuation child

data Moves move = Finished Statistics | Next (HashSet move) deriving (Eq, Ord, Read, Show)

fromMoves :: Hashable move => Moves move -> (Statistics, HashMap move Statistics)
fromMoves (Finished stats) = (stats, mempty)
fromMoves (Next ms) = (mempty, mempty <$ toMap ms)
