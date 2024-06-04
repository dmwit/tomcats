{-# Language LambdaCase #-}
{-# Language MultiWayIf #-}

module Main where

import Control.Monad
import Data.Array.IO
import Data.Foldable
import Data.Functor
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Traversable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Tomcats.AlphaZero.Double as T

data Cell = E | X | O deriving (Eq, Ord, Read, Show, Bounded, Enum)

instance Hashable Cell where
	hashWithSalt s = hashWithSalt s . fromEnum

type Coord = (Int, Int)
type Board = IOArray Coord Cell
type Move = (Cell, Coord)

ticTacToeParameters :: IO (T.RNG, T.Parameters IO Double T.Statistics Move Board)
ticTacToeParameters = T.parametersIO
	5 -- the typical number of legal moves
	0.25 -- how much noise to add to prior probability estimates
	1 -- amount of bias towards prior probabilities (as opposed to observed valuations)
	(mapArray id) -- how to clone a board
	-- what next moves are available, or who won if the game is over
	(\board -> winner board >>= \case
		Just c -> pure . T.won . toPlayer $ c
		Nothing -> evaluateBoard board
	)
	(toPlayer . fst) -- translate between tic-tac-toe moves and tomcats players
	(\board (cell, coord) -> writeArray board coord cell) -- how to play a move

main :: IO ()
main = do
	board <- newArray ((0,0), (2,2)) E
	(g, params) <- ticTacToeParameters
	let loop nMax 0 t = display board t >> T.descend params 1 g board t >>= \case
	    	Nothing -> pure ()
	    	Just (move, t') -> print move >> putStrLn "press ENTER to continue" >> getLine >> loop nMax nMax t'
	    loop nMax n t = T.mcts params board t >>= loop nMax (n-1)
	T.initialize params board >>= loop 1000 1000

triples :: Board -> IO [[Cell]]
triples board = do
	cols <- for [0..2] $ \x -> for [0..2] $ \y -> readArray board (x, y)
	rows <- for [0..2] $ \y -> for [0..2] $ \x -> readArray board (x, y)
	diags <- for [id, (2-)] $ \f -> for [0..2] $ \x -> readArray board (x, f x)
	pure $ cols ++ rows ++ diags

winner :: Board -> IO (Maybe Cell)
winner board = do
	ts <- triples board
	pure $ if
		| [X,X,X] `elem` ts -> Just X
		| [O,O,O] `elem` ts -> Just O
		| otherwise -> Nothing

findEmpties :: Board -> IO [Coord]
findEmpties board = filterM
	(\pos -> (E==) <$> readArray board pos)
	[(x, y) | x <- [0..2], y <- [0..2]]

-- In actual AlphaZero, this function is implemented with a neural net that is
-- trained on self-play. For simplicity, we'll use a simple hand-coded
-- heuristic instead, as follows.
--
-- Valuation estimate: what fraction of "two-in-a-rows" that exist are for X?
--
-- Prior probabilities: a fixed collection of weights for each move, and
-- independent of the current board
evaluateBoard :: Board -> IO (T.Moves Move)
evaluateBoard board = do
	xn <- countTwos board X
	on <- countTwos board O
	es <- findEmpties board
	let valuationEstimate = if null es || xn+on <= 0
	    	then 0.5
	    	else fromIntegral xn / fromIntegral (xn+on)
	    player = if even (length es) then X else O
	pure
		. T.Moves valuationEstimate
		. T.normalize
		. HM.mapKeys ((,) player)
		. HM.intersection moveWeights
		. HS.toMap
		. HS.fromList
		$ es

moveWeights :: HashMap Coord Double
moveWeights = mempty
	<> [(1,1)] ~> 4 -- center
	<> [(x,y) | x <- [0,2], y <- [0,2]] ~> 2 -- corners
	<> [f (1,y) | y <- [0,2], f <- [id, \(x,y) -> (y,x)]] ~> 1 -- edges
	where
	infix 7 ~>
	ps ~> w = HM.fromList [(p, w) | p <- ps]

countTwos :: Board -> Cell -> IO Int
countTwos board c = sum . map twos <$> triples board where
	twos [c0, c1, c2] = length $ filter ((c,c)==) [(c0,c1), (c0,c2), (c1,c2)]

toPlayer :: Cell -> T.Player
toPlayer X = T.I
toPlayer O = T.O
toPlayer E = error "WTF, tried to think of empty as a player"

display :: Board -> T.Tree T.Statistics Move -> IO ()
display board t = do
	putStr "locally: " >> displayStats (T.statistics t)
	for_ (HM.toList (T.children t)) $ \(move, child) ->
		putStr (show move ++ ": ") >> displayStats (T.statistics child)
	putStr "TODO: "
	when (HM.null (T.unexplored t)) (putStr "<nothing left to explore>")
	for_ (HM.toList (T.unexplored t)) $ \(move, _) -> putStr (show move ++ " ")
	putStrLn ""
	for_ [0..2] $ \y -> do
		for_ [0..2] $ \x -> do
			cell <- readArray board (x, y)
			putStr (show cell ++ " ")
		putStrLn ""

displayStats :: T.Statistics -> IO ()
displayStats s = putStrLn $ "visit count " ++ show (T.visitCount s) ++ ", average valuation " ++ show (T.meanValuation s) ++ ", prior probability " ++ show (T.priorProbability s)
