{-# Language LambdaCase #-}
{-# Language ViewPatterns #-}

module Main where

import Control.Monad
import Data.Array.IO
import Data.Foldable
import Data.Functor
import Data.Hashable
import Data.IORef
import Data.List
import Data.Traversable
import System.Environment
import System.Exit
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Tomcats.Vanilla.Multiplayer as T

-- we'll use -1 for empty cells
type Cell = Int
type Coord = (Int, Int)
type Move = (Cell, Coord)

data GameState = GameState
	{ board :: IOArray Coord Cell
	, checkForWinsAround :: IORef Coord
	, turn :: IORef Int
	, playerCount :: Int
	}

ticTacToeParameters :: IO (T.Parameters IO Double (T.Statistics Cell) Move GameState)
ticTacToeParameters = T.parametersIO
		-- make a copy of a game state
		clone
		-- what next moves are available, or who won if the game is over
		(\st -> winner st >>= \case
			Just c -> pure . T.Finished . T.won $ c
			Nothing -> do
				player <- readIORef (turn st) <&> (`mod` playerCount st)
				es <- findEmpties st
				pure $ case es of
					[] -> T.Finished T.drawn
					_ -> T.Next . HS.fromList . map ((,) player) $ es
		)
		-- translate between tic-tac-toe moves and tomcats players
		fst
		-- how to play a move
		(\st (cell, coord) -> do
			writeArray (board st) coord cell
			writeIORef (checkForWinsAround st) coord
			modifyIORef (turn st) (1+)
		)

main :: IO ()
main = do
	pc <- getArgs >>= parseArgs
	st <- newGameState pc
	params <- ticTacToeParameters
	let loop 0 t = display st t >> T.descend params T.visitCount st t >>= \case
	    	Nothing -> pure ()
	    	Just (move, t') -> print move >> putStrLn "press ENTER to continue" >> getLine >> loop nMax t'
	    loop n t = T.mcts params st t >>= loop (n-1)
	    nMax = 100*pc*pc
	T.initialize params st >>= loop nMax

winner :: GameState -> IO (Maybe Cell)
winner st = do
	(xHint, yHint) <- readIORef (checkForWinsAround st)
	runs <- traverse (traverse (readArray (board st)))
		[ [ coord
		  | n <- [-2..2]
		  , let coord@(x, y) = (xHint + n*dx, yHint + n*dy)
		  , 0 <= x && x <= playerCount st
		  , 0 <= y && y <= playerCount st
		  ]
		| (dx, dy) <- [(0,1), (1,0), (1,1), (1,-1)]
		]
	n <- readIORef (turn st)
	let lastPlayer = (n-1) `mod` playerCount st
	pure $ if any (replicate 3 lastPlayer `isInfixOf`) runs
		then Just lastPlayer
		else Nothing

findEmpties :: GameState -> IO [Coord]
findEmpties st = filterM
	(\pos -> ((-1)==) <$> readArray (board st) pos)
	[(x, y) | x <- [0..playerCount st], y <- [0..playerCount st]]

newGameState :: Int -> IO GameState
newGameState playerCount = pure GameState
	<*> newArray ((0,0), (playerCount,playerCount)) (-1)
	<*> newIORef (0,0) -- doesn't really matter what coordinate we put here
	<*> newIORef 0
	<*> pure playerCount

clone :: GameState -> IO GameState
clone st = pure GameState
	<*> mapArray id (board st)
	<*> (readIORef (checkForWinsAround st) >>= newIORef)
	<*> (readIORef (turn st) >>= newIORef)
	<*> pure (playerCount st)

parseArgs :: [String] -> IO Int
parseArgs = \case
	[] -> 3 <$ putStrLn "No player count specified on the command line; defaulting to 3."
	["-h"] -> usage ExitSuccess
	["--help"] -> usage ExitSuccess
	[reads -> [(n, "")]] -> if inRange (2,26) n then pure n else fail "yo be real"
	_ -> usage (ExitFailure 1)

usage :: ExitCode -> IO a
usage code = do
	nm <- getProgName
	putStrLn $ "USAGE: " ++ nm ++ " [N]"
	putStrLn $ "Play a variant of tic-tac-toe with N players."
	exitWith code

display :: GameState -> T.Tree (T.Statistics Cell) Move -> IO ()
display st t = do
	putStr "locally: " >> displayStats (T.statistics t)
	for_ (HM.toList (T.children t)) $ \(move, child) ->
		putStr (show move ++ ": ") >> displayStats (T.statistics child)
	putStr "TODO: "
	when (HM.null (T.unexplored t)) (putStr "<nothing left to explore>")
	for_ (HM.toList (T.unexplored t)) $ \(move, _) -> putStr (show move ++ " ")
	putStrLn ""
	for_ [0..playerCount st] $ \y -> do
		for_ [0..playerCount st] $ \x -> do
			cell <- readArray (board st) (x, y)
			putStr [playerChar cell, ' ']
		putStrLn ""
	n <- readIORef (turn st)
	putStrLn $ playerChar (n `mod` playerCount st) : "'s turn"

displayStats :: T.Statistics Cell -> IO ()
displayStats s = putStrLn $ "visit count " ++ show (T.visitCount s) ++ ", average valuation " ++ show (T.meanValuation s)

playerChar :: Int -> Char
playerChar (-1) = '-'
playerChar n = toEnum (fromEnum 'a' + n)
