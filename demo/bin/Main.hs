{-# Language LambdaCase #-}
{-# Language MultiWayIf #-}

module Main where

import Control.Monad
import Data.Array.IO
import Data.Foldable
import Data.Hashable
import Data.Traversable
import Debug.Trace
import System.Random.MWC (createSystemRandom)
import Tomcats
import qualified Data.HashMap.Strict as HM

data Cell = E | X | O deriving (Eq, Ord, Read, Show, Bounded, Enum)

instance Hashable Cell where
	hashWithSalt s = hashWithSalt s . fromEnum

type Coord = (Int, Int)
type Board = IOArray Coord Cell
type Move = (Cell, Coord)

ticTacToeParameters :: IO (Parameters IO Double VanillaStatistics Move Board)
ticTacToeParameters = do
	g <- createSystemRandom
	pure $ rollout (uniform g) Parameters
		{ score = \(cell, _) parent child -> case cell of
			E -> error "WTF, scoring an empty move, that should never happen"
			X -> ucb1 (visitCount parent) (visitCount child) (valuation child)
			-- higher numbers are better for X, so we must "negate" the
			-- valuation for O; but we also must keep it in the range
			-- [0,visits] for ucb to work right
			O -> ucb1 (visitCount parent) (visitCount child) (visitCount child - valuation child)
		, expand = \board -> do
			w <- winner board
			case w of
				Just X -> pure (vanillaLeaf 1, mempty)
				Just O -> pure (vanillaLeaf 0, mempty)
				_ -> do
					empties <- filterM (\pos -> (E==) <$> readArray board pos) [(x, y) | x <- [0..2], y <- [0..2]]
					case empties of
						[] -> pure (vanillaLeaf 0.5, mempty)
						_ -> pure (mempty, HM.fromList [((player, coord), mempty) | coord <- empties])
							where player = if even (length empties) then X else O
		, clone = mapArray id
		, play = \board (cell, coord) -> writeArray board coord cell
		, preprocess = emptyPreprocessor
		}

winner :: Board -> IO (Maybe Cell)
winner board = do
	cols <- for [0..2] $ \x -> for [0..2] $ \y -> readArray board (x, y)
	rows <- for [0..2] $ \y -> for [0..2] $ \x -> readArray board (x, y)
	diags <- for [id, (2-)] $ \f -> for [0..2] $ \x -> readArray board (x, f x)
	let triples = cols ++ rows ++ diags
	pure $ if
		| [X,X,X] `elem` triples -> Just X
		| [O,O,O] `elem` triples -> Just O
		| otherwise -> Nothing

main :: IO ()
main = do
	board <- newArray ((0,0), (2,2)) E
	params <- ticTacToeParameters
	let loop nMax 0 t = display board t >> descend params visitCount board t >>= \case
	    	Nothing -> pure ()
	    	Just (move, t') -> print move >> putStrLn "press ENTER to continue" >> getLine >> loop nMax nMax t'
	    loop nMax n t = mcts params board t >>= loop nMax (n-1)
	initialize params board >>= loop 1000 1000

display :: Board -> Tree VanillaStatistics Move -> IO ()
display board t = do
	putStr "locally: " >> displayStats (statistics t)
	for_ (HM.toList (children t)) $ \(move, t) ->
		putStr (show move ++ ": ") >> displayStats (statistics t)
	putStr "TODO: "
	when (HM.null (unexplored t)) (putStr "<nothing left to explore>")
	for_ (HM.toList (unexplored t)) $ \(move, _) -> putStr (show move ++ " ")
	putStrLn ""
	for_ [0..2] $ \y -> do
		for_ [0..2] $ \x -> do
			cell <- readArray board (x, y)
			putStr (show cell ++ " ")
		putStrLn ""

displayStats :: VanillaStatistics -> IO ()
displayStats s = putStrLn $ "visit count " ++ show (visitCount s) ++ ", average valuation " ++ show (meanValuation s)
