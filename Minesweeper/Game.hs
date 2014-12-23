module Minesweeper.Game where

import System.Random
import Minesweeper.Cell
import Minesweeper.Board
import Minesweeper.Difficulty

uncover :: Board -> (Int, Int) -> Board
uncover board coord = replaceCell board coord newCell
  where newCell = (getCell board coord) { revealed = True }

flag :: Board -> (Int, Int) -> Board
flag board coord = replaceCell board coord newCell
  where newCell = (getCell board coord) { flagged = True }

unflag :: Board -> (Int, Int) -> Board
unflag board coord = replaceCell board coord newCell
  where newCell = (getCell board coord) { flagged = False }

main :: IO()
main = do
  -- Game setup
  gen <- newStdGen -- random number generator seed
  let difficulty = beginner
  let board = generateRandomBoard gen difficulty

  putStrLn $ showBoard board

  putStrLn $ showTrueBoard board