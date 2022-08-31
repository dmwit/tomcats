{-# Language GADTs #-}
{-# Language RankNTypes #-}

module Tomcats (
	-- * Top level
	mcts,
	-- * Parameters
	Parameters(..),
	emptyPreprocessor, rollout,
	-- * State
	Tree(..),
	initialize, unsafeInitialize,
	descend, unsafeDescend,
	-- * Utilities
	ucb1, uniform,
	) where

import Control.Monad
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Traversable
import System.Random.Stateful (StatefulGen(uniformWord64R))
import qualified Data.HashMap.Strict as HM

-- | A representation of the current state in a run of a Monte Carlo tree
-- search.
--
-- This gets kinda big, so you might want to avoid the 'Eq' and 'Ord'
-- instances...
data Tree stats move = Tree
	{ statistics :: stats
	, children :: HashMap move (Tree stats move)
	, unexplored :: HashMap move stats
	, cachedEvaluation :: Maybe stats
	-- ^ Used only at leaf nodes, and not really for public consumption (nor
	-- public production)
	} deriving (Eq, Ord, Read, Show)

data Parameters m score stats move position where
	Parameters :: (Monad m, Ord score, Semigroup stats, Hashable move) =>
		{ score :: move -> stats -> stats -> score
		-- ^ Given a move and the statistics for the parent node and current node,
		-- compute a priority score for searching this node in the future. Larger
		-- scores are searched first.
		--
		-- See also 'ucb1'.
		, expand :: position -> m (stats, HashMap move stats)
		-- ^ Compute the moves available from this position. Return an empty
		-- hashmap to indicate that this is a leaf node (i.e. a won or lost
		-- position). The stats returned in the first part should be correct if
		-- this is a leaf node, but may be an estimate (e.g. as computed by a
		-- neural net or doing a rollout) if not.
		--
		-- Implementers may mutate the given position.
		, clone :: position -> m position
		-- ^ Make a copy of a position. Implementers should ensure that mutations
		-- to the argument are not visible to the position returned and vice versa.
		, play :: position -> move -> m ()
		-- ^ Mutate the current position, making the given move.
		--
		-- Implementers may assume that the position given would 'expand' to a
		-- non-empty hashmap.
		, preprocess :: position -> Tree stats move -> m (stats, Tree stats move)
		-- ^ Make adjustments to a game tree before searching it. Under normal
		-- circumstances this should be 'emptyPreprocessor'.
		--
		-- While descending into a game tree during an iteration of 'mcts',
		-- 'preprocess' will be used on each node to make arbitrary adjustments
		-- to the tree. Implementers should not modify the position provided.
		-- Any statistics produced here will be included in the current node
		-- and backpropagated to its ancestors (using the 'Semigroup' instance,
		-- as usual, in both cases).
		} -> Parameters m score stats move position

-- | Create a suitable initial tree for passing to 'mcts'.
initialize ::
	Parameters m score stats move position ->
	position -> m (Tree stats move)
initialize params@Parameters{} = clone params >=> unsafeInitialize params

-- | Just like initialize, except that it might mutate the position it's given.
unsafeInitialize ::
	Parameters m score stats move position ->
	position -> m (Tree stats move)
unsafeInitialize params@Parameters{} pos = do
	(stats, moves) <- expand params pos
	pure Tree
		{ statistics = stats
		, children = HM.empty
		, unexplored = moves
		, cachedEvaluation = if HM.null moves then Just stats else Nothing
		}

-- | Choose a move using the currently available information, and mutate the
-- current position accordingly. The move that maximizes the given summary
-- function will be selected. This returns 'Nothing' when the position is at a
-- leaf node.
descend :: Ord a =>
	Parameters m score stats move position ->
	(stats -> a) ->
	position -> Tree stats move ->
	m (Maybe (move, Tree stats move))
descend params@Parameters{} summarize pos t = for
	(maximumOn (const summarize) ((statistics <$> children t) `HM.union` unexplored t))
	(\(move, _, _) -> (,) move <$> unsafeDescend params move pos t)

-- | Mutate the current position, descending down a particular branch of the
-- tree (and discarding all the other branches). Callers must guarantee that
-- the given move is legal in the current position. Returns the subtree rooted
-- at that branch.
unsafeDescend ::
	Parameters m score stats move position ->
	move ->
	position -> Tree stats move ->
	m (Tree stats move)
unsafeDescend params@Parameters{} move pos t = do
	play params pos move
	case HM.lookup move (children t) of
		Nothing -> initialize params pos
		Just t' -> pure t'

-- | Do no 'preprocess'ing at all.
emptyPreprocessor ::
	(Applicative m, Monoid stats) =>
	position -> tree -> m (stats, tree)
emptyPreprocessor _ t = pure (mempty, t)

-- | One standard-ish way of implementing 'expand' is to explore random branches
-- from the given position and take the valuation of the leaf node you reach
-- this way as the estimate of this position's outcome. This function extends
-- an incomplete expansion function -- one that only reports a valuation for
-- leaves and only reports a set of moves for internal nodes -- into a complete
-- one.
--
-- See also 'uniform'.
rollout ::
	Monoid stats =>
	-- | Implementers of this argument may assume the map is nonempty.
	(forall a. position -> HashMap move a -> m move) ->
	Parameters m score stats move position ->
	Parameters m score stats move position
rollout select params@Parameters{} = params { expand = go } where
	go pos = do
		old <- expand params pos
		let loop (stats, ms)
		    	| HM.null ms = pure stats
		    	| otherwise = do
		    		m <- select pos ms
		    		play params pos m
		    		expand params pos >>= loop
		stats <- loop old
		pure (stats, mempty <$ snd old)

-- | Perform one iteration of Monte Carlo tree search, choosing a leaf node and
-- expanding it. You should iterate this until you run out of computational
-- budget.
mcts ::
	Parameters m score stats move position ->
	position -> Tree stats move ->
	m (Tree stats move)
mcts params@Parameters{} pos t = do
	pos' <- clone params pos
	snd <$> mcts_ params pos' t

maximumOn :: Ord a => (k -> v -> a) -> HashMap k v -> Maybe (k, v, a)
-- checking for emptiness once at the beginning is cheaper than re-checking on
-- every iteration, as you would have to do if you folded with a Maybe
maximumOn f m = case HM.toList m of
	[] -> Nothing
	(k,v):_ -> Just $ HM.foldlWithKey'
		(\old@(k,v,a) k' v' -> let a' = f k' v' in if a' > a then (k',v',a') else old)
		(k, v, f k v)
		m

mcts_ ::
	Parameters m score stats move position ->
	position ->
	Tree stats move ->
	m (stats, Tree stats move)
mcts_ params@Parameters{} pos = go where
	go t_ = do
		(dstats, t) <- preprocess params pos t_
		case ( maximumOn (\m t'    -> score params m (statistics t) (statistics t')) (children t)
		     , maximumOn (\m stats -> score params m (statistics t) stats          ) (unexplored t)
		     ) of
			(Just (m1, t1, score1), Just (m2, stats2, score2))
				| score1 < score2 -> explore dstats t m2 stats2
				| otherwise -> recurse dstats t m1 t1
			(Just (m1, t1, _score1), _) -> recurse dstats t m1 t1
			(_, Just (m2, stats2, _score2)) -> explore dstats t m2 stats2
			_ -> case cachedEvaluation t of
				Just eval -> pure (eval, t { statistics = statistics t <> dstats <> eval })
				Nothing -> do -- should never happen
					(eval, _) <- expand params pos
					let bothStats = dstats <> eval
					pure (bothStats, t { statistics = statistics t <> bothStats, cachedEvaluation = Just eval })

	explore dstats t m mstats = do
		play params pos m
		child <- unsafeInitialize params pos
		let bothStats = dstats <> statistics child
		pure (bothStats, t
			{ statistics = statistics t <> bothStats
			, children = HM.insert m child (children t)
			, unexplored = HM.delete m (unexplored t)
			})

	recurse dstats t m child = do
		play params pos m
		(stats, child') <- go child
		let bothStats = dstats <> stats
		pure (bothStats, t
			{ statistics = statistics t <> bothStats
			, children = HM.insert m child' (children t)
			})

-- | Compute the popular upper-confidence bound score.
--
-- Arguments are a visit count for the parent node (n in the literature), a
-- visit count for the current node (n_i in the literature), and a cumulative
-- utility achieved by children of the current node (Q_i in the literature).
-- The utility of individual leaves should be in the range [0, 1] (so that 0 <=
-- Q_i <= n_i).
ucb1 :: Double -> Double -> Double -> Double
ucb1 _ 0 _ = 1/0
ucb1 n n_i q_i = q_i/n_i + sqrt (2 * log n / n_i)

-- | Choose uniformly at random from among a collection of moves. Note that the
-- @position@ and @stats@ type variables are completely unconstrained. Though
-- the type is not what you would normally choose for this functionality, it
-- fits smoothly as an argument to 'rollout'.
uniform :: StatefulGen g m => g -> position -> HashMap move stats -> m move
uniform g _pos moves = do
	ix <- uniformWord64R (fromIntegral (HM.size moves) - 1) g
	pure . fst $ HM.toList moves !! fromIntegral ix

data VanillaStatistics = VanillaStatistics
	{ visitCount, valuation :: {-# UNPACK #-} !Double
	} deriving (Eq, Ord, Read, Show)

-- | A suitable statistic for a leaf node. The argument should be in the range
-- [0,1].
vanillaLeaf :: Double -> VanillaStatistics
vanillaLeaf v = VanillaStatistics 1 v

meanValuation :: VanillaStatistics -> Double
meanValuation stats = valuation stats / visitCount stats

instance Semigroup VanillaStatistics where
	a <> b = VanillaStatistics (visitCount a + visitCount b) (valuation a + valuation b)

instance Monoid VanillaStatistics where mempty = VanillaStatistics 0 0
