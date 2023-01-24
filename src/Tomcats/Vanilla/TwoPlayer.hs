module Tomcats.Vanilla.TwoPlayer (
	-- * Top level
	parameters, parametersIO,
	T.mcts, T.initialize, descend, T.Tree(..),
	-- * Statistics and moves
	Moves(..),
	Statistics(..),
	won, lost, drawn,
	meanValuation,
	-- * Other
	Player(..), otherPlayer,
	wonValuation, lostValuation, drawnValuation,
	ucb1,
	T.Parameters(..),
	) where

import Data.Aeson
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet, toMap)
import System.Random.Stateful
import System.Random.MWC (createSystemRandom)

import qualified Tomcats as T
import Tomcats.TwoPlayer

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
	-- | 'T.play'
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

-- | Choose the move that has the most visits, and mutate the current position
-- accordingly. This returns 'Nothing' when the position is at a leaf node.
descend ::
	T.Parameters m score Statistics move position ->
	position -> T.Tree Statistics move ->
	m (Maybe (move, T.Tree Statistics move))
descend params = T.descend params visitCount

data Statistics = Statistics
	{ visitCount, cumulativeValuation :: {-# UNPACK #-} !Double
	} deriving (Eq, Ord, Read, Show)

meanValuation :: Statistics -> Double
meanValuation stats = cumulativeValuation stats / visitCount stats

instance Semigroup Statistics where
	Statistics c v <> Statistics c' v' = Statistics (c+c') (v+v')

instance Monoid Statistics where mempty = Statistics 0 0

instance ToJSON Statistics where toJSON (Statistics vc cv) = toJSON (vc, cv)
instance FromJSON Statistics where parseJSON v = uncurry Statistics <$> parseJSON v

-- | Suitable for use as a 'T.score'; compose with a function of type @move ->
-- Player@. This is already called on your behalf if you use the default
-- behaviors provided by 'parameters' or 'parametersIO'.
ucb1 :: Player -> Statistics -> Statistics -> Double
ucb1 p parent child = T.ucb1 (visitCount parent) (visitCount child) $ case p of
	O -> visitCount child - cumulativeValuation child
	I -> cumulativeValuation child

data Moves move
	= Finished Double
	-- ^ The game is over, and this is the valuation.
	| Next (HashSet move)
	-- ^ The game is afoot!
	deriving (Eq, Ord, Read, Show)

-- | @won p@ is a suitable summary of a game that's finished and won by @p@.
won :: Player -> Moves move
won = Finished . wonValuation

-- | @lost p@ is a suitable summary of a game that's finished and lost by @p@.
-- This is equivalent to combining 'won' and 'otherPlayer'.
lost :: Player -> Moves move
lost = Finished . lostValuation

-- | A suitable summary of a game that's finished where neither player won.
drawn :: Moves move
drawn = Finished drawnValuation

fromMoves :: Hashable move => Moves move -> (Statistics, HashMap move Statistics)
fromMoves (Finished valuation) = (Statistics 1 valuation, mempty)
fromMoves (Next ms) = (mempty, mempty <$ toMap ms)
