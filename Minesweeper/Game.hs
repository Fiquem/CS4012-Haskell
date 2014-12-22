module Minesweeper.Game where

import System.Random
import Minesweeper.Board
import Minesweeper.Difficulty

main :: IO()
main = do
	-- Game setup
	gen <- getStdGen -- random number generator seed
	let difficulty = beginner
	let minesBoard = generateRandomBoard gen difficulty

	putStrLn $ showBoard minesBoard