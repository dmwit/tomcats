module Tomcats.Vanilla.Multiplayer (
	-- * Top level
	parameters, parametersIO,
	T.mcts, T.initialize, descend, T.Tree(..),
	-- * Statistics and moves
	Moves(..),
	Statistics(..),
	won, lost, drawn,
	meanValuation,
	-- * Other
	ucb1,
	wonValuation, lostValuation, drawnValuation,
	T.Parameters(..),
	) where

import Control.Applicative
import Data.Aeson
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

-- | Choose the move that has the most visits, and mutate the current position
-- accordingly. This returns 'Nothing' when the position is at a leaf node.
descend ::
	T.Parameters m score (Statistics player) move position ->
	position -> T.Tree (Statistics player) move ->
	m (Maybe (move, T.Tree (Statistics player) move))
descend params = T.descend params visitCount

data Statistics player = Statistics
	{ visitCount :: {-# UNPACK #-} !Double
	, cumulativeValuations :: TMap player Double
	} deriving Show

instance ToJSONKey player => ToJSON (Statistics player) where toJSON (Statistics vc cv) = toJSON (vc, cv)
instance (FromJSONKey player, Hashable player) => FromJSON (Statistics player) where parseJSON v = uncurry Statistics <$> parseJSON v

-- | @won p@ is a suitable valuation for a game that's finished and won by @p@.
wonValuation :: Hashable player => player -> TMap player Double
wonValuation p = TMap.singleton p 1 0

-- | @lost p@ is a suitable valuation for a game that's finished and won by
-- everybody but @p@.
lostValuation :: Hashable player => player -> TMap player Double
lostValuation p = TMap.singleton p 0 1

-- | @drawn@ is a suitable valuation for a game that's finished, but nobody
-- really won or lost.
drawnValuation :: Hashable player => TMap player Double
drawnValuation = 0.5

meanValuation :: Statistics player -> TMap player Double
meanValuation stats = cumulativeValuations stats <&> (/visitCount stats)

instance Hashable player => Semigroup (Statistics player) where
	Statistics c v <> Statistics c' v' = Statistics (c+c') (v+v')

instance Hashable player => Monoid (Statistics player) where mempty = Statistics 0 0

-- | Suitable for use as a 'T.score'; compose with a function of type @move ->
-- player@. This is already called on your behalf if you use the default
-- behaviors provided by 'parameters' or 'parametersIO'.
ucb1 :: Hashable player => player -> Statistics player -> Statistics player -> Double
ucb1 p parent child = T.ucb1 (visitCount parent) (visitCount child) (cumulativeValuations child TMap.! p)

data Moves move player
	= Finished (TMap player Double)
	-- ^ The game is over, and this is the valuation.
	| Next (HashSet move)
	-- ^ The game is afoot!
	deriving Show

-- | @won p@ is a suitable summary of a game that's finished and won by @p@.
won :: Hashable player => player -> Moves move player
won = Finished . wonValuation

-- | @lost p@ is a suitable summary of a game that's finished and won by
-- everybody but @p@.
lost :: Hashable player => player -> Moves move player
lost = Finished . lostValuation

-- | A suitable summary of a game that's finished, but where nobody really won
-- or lost.
drawn :: Hashable player => Moves move player
drawn = Finished drawnValuation

fromMoves :: (Hashable move, Hashable player) => Moves move player -> (Statistics player, HashMap move (Statistics player))
fromMoves (Finished valuation) = (Statistics 1 valuation, mempty)
fromMoves (Next ms) = (mempty, mempty <$ toMap ms)
