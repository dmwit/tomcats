{-# Language LambdaCase #-}
{-# Language MultiWayIf #-}

module Main where

import Control.Monad
import Data.Array.IO
import Data.Foldable
import Data.Functor
import Data.Hashable
import Data.Traversable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Tomcats.Vanilla.TwoPlayer as T

data Cell = E | X | O deriving (Eq, Ord, Read, Show, Bounded, Enum)

instance Hashable Cell where
	hashWithSalt s = hashWithSalt s . fromEnum

type Coord = (Int, Int)
type Board = IOArray Coord Cell
type Move = (Cell, Coord)

ticTacToeParameters :: IO (T.Parameters IO Double T.Statistics Move Board)
ticTacToeParameters = T.parametersIO
		(mapArray id) -- how to clone a board
		-- what next moves are available, or who won if the game is over
		(\board -> winner board >>= \case
			Just c -> pure . T.won . toPlayer $ c
			Nothing -> findEmpties board <&> \case
				[] -> T.drawn
				empties -> T.Next . HS.fromList . map ((,) player) $ empties
					where player = if even (length empties) then X else O
		)
		(toPlayer . fst) -- translate between tic-tac-toe moves and tomcats players
		(\board (cell, coord) -> writeArray board coord cell) -- how to play a move

main :: IO ()
main = do
	board <- newArray ((0,0), (2,2)) E
	params <- ticTacToeParameters
	let loop nMax 0 t = display board t >> T.descend params board t >>= \case
	    	Nothing -> pure ()
	    	Just (move, t') -> print move >> putStrLn "press ENTER to continue" >> getLine >> loop nMax nMax t'
	    loop nMax n t = T.mcts params board t >>= loop nMax (n-1)
	T.initialize params board >>= loop 1000 1000

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

findEmpties :: Board -> IO [Coord]
findEmpties board = filterM
	(\pos -> (E==) <$> readArray board pos)
	[(x, y) | x <- [0..2], y <- [0..2]]

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
displayStats s = putStrLn $ "visit count " ++ show (T.visitCount s) ++ ", average valuation " ++ show (T.meanValuation s)
