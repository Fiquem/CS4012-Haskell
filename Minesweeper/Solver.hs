module Minesweeper.Solver (
	autoMove
) where

import Minesweeper.Cell
import Minesweeper.Board
import Minesweeper.Difficulty
import Minesweeper.Game

autoMove :: Board -> Difficulty -> Board
autoMove board difficulty = 
  if fst result
    then snd result
    else board -- uncover board (head $ getUnrevealedCells board) -- reveal next unrevealed cell
  where result = makeSafeMove board

makeSafeMove :: Board -> (Bool, Board)
makeSafeMove board = attemptOneSafeMove board (getRevealedCells board) 

attemptOneSafeMove :: Board -> [Cell] -> (Bool, Board)
attemptOneSafeMove board [] = (False, board)
attemptOneSafeMove board (x:xs) = 
  if fst result
    then result
    else attemptOneSafeMove board xs -- didn't find a safe move let's continue looking
  where result = flagToAdjacent board x

uncoverAdjacent :: Board -> Cell -> (Bool, Board)
uncoverAdjacent board cell =
  if (count == 0)
    then (True, uncover board $ head unmarked)
    else (False, board)
  where
    count = length flagged
    unmarked = unmarkedAdjacentCells board cell
    flagged = flaggedAdjacentCells board cell

flagToAdjacent :: Board -> Cell -> (Bool, Board)
flagToAdjacent board cell =
  if (count <= (adjacentMines cell)) && (count > 0)
    then (True, flag board $ head unmarked) -- apply only one move flag
    else (False, board)
  where 
    count = length unmarked
    unmarked = unmarkedAdjacentCells board cell

unmarkedAdjacentCells :: Board -> Cell -> [Cell]
unmarkedAdjacentCells board cell = filter (\x -> ((not $ revealed x) || (not $ flagged x))) $ getAdjacentCells board cell

flaggedAdjacentCells :: Board -> Cell -> [Cell]
flaggedAdjacentCells board cell = filter (\x -> flagged x) $ getAdjacentCells board cell

getRevealedCells :: Board -> [Cell]
getRevealedCells board = filter (\x -> revealed x) (getAllCells board)

getUnrevealedCells :: Board -> [Cell]
getUnrevealedCells board = filter (\x -> not $ revealed x) (getAllCells board)
