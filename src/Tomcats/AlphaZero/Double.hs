{-# Language LambdaCase #-}

module Tomcats.AlphaZero.Double (
	-- * Top level
	parameters, parametersIO, parametersDeterministic,
	T.mcts, T.initialize, descend, descendDeterministic, T.Tree(..),
	-- * Statistics and moves
	Moves(..),
	finished,
	normalize,
	Statistics(..),
	won, lost, drawn,
	meanValuation,
	normalizeStatistics,
	-- * Other
	Player(..), otherPlayer,
	wonValuation, lostValuation, drawnValuation,
	pucbA0, pucbA0Raw, dirichlet, dirichletA0,
	RNG(..),
	T.Parameters(..),
	unzero, lerp,
	) where

import Control.Monad
import Control.Monad.Trans.State
import Data.Aeson
import Data.Functor
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified System.Random.MWC.Distributions as Dist

import qualified Tomcats as T
import Tomcats.AlphaZero.Shared
import Tomcats.TwoPlayer

parameters :: (Hashable move, StatefulGen g m) =>
	-- | The typical number of legal moves.
	Double ->
	-- | How strongly to randomize during expansion. Zero means don't
	-- randomize; one means replace the distribution produced with pure noise.
	-- If you're not sure, 0.25 is a sensible default.
	Double ->
	-- | c_{puct}; larger values bias the search more and more towards prior
	-- probabilities; zero ignores the probabilities. Maybe try something in
	-- the 1-2 range if you're not sure what to pick.
	Double ->
	-- | 'T.clone'
	(position -> m position) ->
	-- | If the game is over, give the valuation of the final position.
	-- Otherwise, give an estimate of the valuation the game will have when it
	-- finishes, and an estimate of the probabilities that each legal move is
	-- the best one.
	(position -> m (Moves move)) ->
	-- | Tell which player made a move (so we know whether to strive for low
	-- valuations or high valuations).
	(move -> Player) ->
	-- | 'T.play'
	(position -> move -> m ()) ->
	g -> T.Parameters m Double Statistics move position
parameters numMoves alpha c_puct clone moves player play g =
	(parametersDeterministic c_puct clone impossible player play)
		{ T.expand = moves >=> fromMoves numMoves alpha g
		}
	where impossible = error "The impossible happened in parameters: parametersDeterministic used its moves argument somewhere other than in expand"

-- | See 'parameters' for a breakdown of arguments. This function is just like
-- that one, but it initializes an RNG on your behalf. It also returns the RNG
-- in case you would like to use it with 'descend'.
parametersIO :: Hashable move =>
	Double ->
	Double ->
	Double ->
	(position -> IO position) ->
	(position -> IO (Moves move)) ->
	(move -> Player) ->
	(position -> move -> IO ()) ->
	IO (RNG, T.Parameters IO Double Statistics move position)
parametersIO numMoves alpha c_puct clone moves player play =
	-- if you change createSystemRandom here, double-check that the haddocks on
	-- RNG are still correct
	liftM2 (,) RNG (parameters numMoves alpha c_puct clone moves player play) <$> createSystemRandom

-- | See also 'parameters', which accepts the same arguments and a few extras.
-- Generally, you should use 'parameters' or 'parametersIO' during training and
-- 'parametersDeterministic' when you want to play for real.
parametersDeterministic :: (Hashable move, Monad m) =>
	-- | c_{puct}; larger values bias the search more and more towards prior
	-- probabilities; zero ignores the probabilities. Maybe try something in
	-- the 1-2 range if you're not sure what to pick.
	Double ->
	-- | 'T.clone'
	(position -> m position) ->
	-- | If the game is over, give the valuation of the final position.
	-- Otherwise, give an estimate of the valuation the game will have when it
	-- finishes, and an estimate of the probabilities that each legal move is
	-- the best one.
	(position -> m (Moves move)) ->
	-- | Tell which player made a move (so we know whether to strive for low
	-- valuations or high valuations).
	(move -> Player) ->
	-- | 'T.play'
	(position -> move -> m ()) ->
	T.Parameters m Double Statistics move position
parametersDeterministic c_puct clone moves player play = T.Parameters
	{ T.score = pucbA0 c_puct . player
	, T.expand = fmap fromMovesDeterministic . moves
	, T.clone = clone
	, T.play = play
	, T.preprocess = T.emptyPreprocessor
	}

-- | Sample a move from a distribution determined by the currently available
-- information, and mutate the current position accordingly. This returns
-- 'Nothing' when the position is at a leaf node.
descend :: StatefulGen g m =>
	T.Parameters m score Statistics move position ->
	-- | Temperature, which is a parameter that controls exploration. At a
	-- temperature of 0, all legal moves are equally likely; the limit as the
	-- temperature approaches infinity is to deterministically choose the move
	-- that was visited the most. If you're not sure what to choose, 1 is a
	-- fine choice.
	--
	-- See also 'descendDeterministic' for infinite temperature.
	Double -> g ->
	position -> T.Tree Statistics move ->
	m (Maybe (move, T.Tree Statistics move))
descend params@T.Parameters{} temperature g pos t = case weights of
	[] -> pure Nothing
	_ -> do
		move <- (moves!!) <$> Dist.categorical (V.fromList weights) g
		Just . (,) move <$> T.unsafeDescend params move pos t
	where
	weight stats = visitCount stats ** temperature
	-- weight <$> unexplored t will usually be all-zeros... but not when the
	-- temperature is zero
	(moves, weights) = unzip . HM.toList $
		(weight . T.statistics <$> T.children t) `HM.union`
		(weight <$> T.unexplored t)

-- | Choose the move that has the most visits, and mutate the current position
-- accordingly. This returns 'Nothing' when the position is at a leaf node.
--
-- See also 'descend'. Generally, you should use 'descend' during training and
-- 'descendDeterministic' when you want to play for real.
descendDeterministic ::
	T.Parameters m score Statistics move position ->
	position -> T.Tree Statistics move ->
	m (Maybe (move, T.Tree Statistics move))
descendDeterministic params = T.descend params visitCount

data Statistics = Statistics
	{ visitCount, priorProbability, cumulativeValuation :: {-# UNPACK #-} !Double
	}
	deriving (Eq, Ord, Read, Show)

instance Semigroup Statistics where
	Statistics vc pp cv <> Statistics vc' pp' cv' = Statistics
		{ visitCount = vc + vc'
		-- I know it looks weird to add probabilities. The plan is that the
		-- statistics that are propagated up the tree always have 0 here, so
		-- that the initial value computed by expanding a position remains till
		-- the end of time.
		, priorProbability = pp + pp'
		, cumulativeValuation = cv + cv'
		}

instance Monoid Statistics where mempty = Statistics 0 0 0

instance ToJSON Statistics where toJSON stats = toJSON (visitCount stats, priorProbability stats, cumulativeValuation stats)
instance FromJSON Statistics where parseJSON v = parseJSON v <&> \(vc, pp, cv) -> Statistics vc pp cv

meanValuation :: Statistics -> Double
meanValuation stats = cumulativeValuation stats / unzero (visitCount stats)

-- | Suitable for use as a 'T.score'; compose with a function of type @move ->
-- Player@. This is already called on your behalf if you use the default
-- behaviors provided by 'parameters', 'parametersIO', or
-- 'parametersDeterministic'.
--
-- The first argument is a parameter that controls exploration, called c_{puct}
-- in Mastering the Game of Go Without Human Knowledge; larger values bias the
-- search more and more towards prior probabilities. Setting it to something
-- negative probably isn't sensible; it would cause the search to actively
-- avoid moves with high prior probabilities.
pucbA0 :: Double -> Player -> Statistics -> Statistics -> Double
pucbA0 c_puct p parent child = pucbA0Raw c_puct (priorProbability child) (visitCount parent) (visitCount child) $ case p of
	O -> visitCount child - cumulativeValuation child
	I -> cumulativeValuation child

data Moves move = Moves
	{ valuation :: {-# UNPACK #-} !Double
	-- ^ If the game is over, this should be the valuation of the final
	-- position. Otherwise, it should be an estimate of what the final
	-- valuation will be when the game finishes.
	, moveDistribution :: HashMap move Double
	-- ^ If the game is over, this should be empty; otherwise, it should be a
	-- distribution on moves (and so should be a collection of positive numbers
	-- that sum to one).
	--
	-- See also 'normalize'.
	} deriving (Eq, Ord, Read, Show)

-- | Declare the game finished with the given valuation.
finished :: Double -> Moves move
finished v = Moves v HM.empty

-- | @won p@ is a suitable summary of a game that's finished and won by @p@.
won :: Player -> Moves move
won = finished . wonValuation

-- | @lost p@ is a suitable summary of a game that's finished and lost by @p@.
-- This is equivalent to combining 'won' and 'otherPlayer'.
lost :: Player -> Moves move
lost = finished . lostValuation

-- | A suitable summary of a game that's finished where neither player won.
drawn :: Moves move
drawn = finished drawnValuation

fromMoves :: (Hashable move, StatefulGen g m) => Double -> Double -> g -> Moves move -> m (Statistics, HashMap move Statistics)
fromMoves numMoves alpha g ms = (,) (Statistics 1 0 (valuation ms))
	<$> if HM.size (moveDistribution ms) == 0
	    then pure mempty
	    else dirichletA0 numMoves alpha g (moveDistribution ms)

fromMovesDeterministic :: Hashable move => Moves move -> (Statistics, HashMap move Statistics)
fromMovesDeterministic ms = (,) (Statistics 1 0 (valuation ms))
	$ if HM.size (moveDistribution ms) == 0
	  then mempty
	  else (\pp -> mempty { priorProbability = pp }) <$> moveDistribution ms

-- | Given a collection of non-negative weights, produce a probability
-- distribution by dividing out the sum of the weights. Leaves the map
-- unchanged if the weights sum to 0.
normalize :: HashMap a Double -> HashMap a Double
normalize m = m <&> (/unzero (sum m))

-- | Like 'normalize', turn a collection of non-negative prior weights to a
-- probability distribution; but operates on the 'priorProbability' field of
-- 'Statistics'.
normalizeStatistics :: HashMap a Statistics -> HashMap a Statistics
normalizeStatistics m = m <&> \stats -> stats { priorProbability = priorProbability stats / unzero total } where
	Sum total = foldMap (Sum . priorProbability) m

-- | Compute the AlphaZero variant of the predictor-biased upper confidence
-- bound score.
--
-- The first argument is a parameter that controls exploration, called c_{puct}
-- in Mastering the Game of Go Without Human Knowledge; larger values bias the
-- search more and more towards prior probabilities. Setting it to something
-- negative probably isn't sensible; it would cause the search to actively
-- avoid moves with high prior probabilities.
--
-- The remaining arguments are as in 'T.pucb'.
pucbA0Raw :: Double -> Double -> Double -> Double -> Double -> Double
pucbA0Raw c_puct p n 0 _ = c_puct * p * sqrt n
pucbA0Raw c_puct p n n_i q_i = q_i/n_i + c_puct * p * sqrt n / (1 + n_i)

-- | @\case 0 -> 1; v -> v@
unzero :: Double -> Double
unzero = \case 0 -> 1; v -> v

-- | Given a parameter for a Dirichlet distribution, take a sample and combine
-- the results with each of the elements in the map. This is already called on
-- your behalf if you use the default behaviors provided by 'parameters' or
-- 'parametersIO'.
dirichlet :: StatefulGen g m => Double -> (Double -> v -> v') -> g -> HashMap k v -> m (HashMap k v')
dirichlet param f g m = do
	d <- Dist.dirichlet (V.replicate (HM.size m) param) g
	pure $ evalState (traverse (\v -> state (\i -> (f (d V.! i) v, i+1))) m) 0

-- | @dirichletA0 numMoves alpha g m@ samples from a Dirichlet distribution and
-- folds the result into move distribution @m@. @numMoves@ should be the
-- typical number of legal moves available in your game; @10/numMoves@ is then
-- used as the parameter for the Dirichlet distribution. @alpha@ controls the
-- interpolation between the new sample and the existing move distribution, and
-- should be in the range [0,1]; @1@ means to use the new sample and throw away
-- the existing distribution, and @0@ means to use the existing distribution
-- and throw away the new sample.
dirichletA0 :: StatefulGen g m => Double -> Double -> g -> HashMap move Double -> m (HashMap move Statistics)
dirichletA0 numMoves alpha = dirichlet (10/unzero numMoves) $ \noise pp -> mempty { priorProbability = lerp alpha noise pp }

-- | @lerp alpha x y@ linearly interpolates between @x@ (when @alpha@ is @1@) and @y@ (when @alpha@ is @0@).
lerp :: Double -> Double -> Double -> Double
lerp alpha x y = alpha*x + (1-alpha)*y
