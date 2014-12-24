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
    else uncover board (head $ getUnrevealedCells board) -- reveal next unrevealed cell
  where result = attemptSafeMove board

attemptSafeMove :: Board -> (Bool, Board)
attemptSafeMove board = 
  if fst result
    then result
    else findOneSafeMove board uncoverAdjacent (getRevealedCells board)
  where result = findOneSafeMove board flagAdjacent (getRevealedCells board) 

findOneSafeMove :: Board -> (Board -> Cell -> (Bool, Board)) -> [Cell] -> (Bool, Board)
findOneSafeMove board f [] = (False, board)
findOneSafeMove board f (x:xs) = 
  if fst result
    then result
    else findOneSafeMove board f xs -- didn't find a safe move let's continue looking
  where result = f board x

uncoverAdjacent :: Board -> Cell -> (Bool, Board)
uncoverAdjacent board cell = searchAdjacent board cell uncover flagged unmarked 
  where
    flagged = flaggedAdjacentCells board cell
    unmarked = unmarkedAdjacentCells board cell

flagAdjacent :: Board -> Cell -> (Bool, Board)
flagAdjacent board cell = searchAdjacent board cell flag unmarked unmarked 
  where unmarked = unmarkedAdjacentCells board cell

searchAdjacent :: Board -> Cell -> (Board -> Cell -> Board) -> [Cell] -> [Cell] -> (Bool, Board)
searchAdjacent board cell f countList choiceList = 
  if (count == (adjacentMines cell)) && ((length choiceList) > 0)
    then (True, f board $ head choiceList)
    else (False, board)
  where
    count = length countList


unmarkedAdjacentCells :: Board -> Cell -> [Cell]
unmarkedAdjacentCells board cell = filter (\x -> ((not $ revealed x) && (not $ flagged x))) $ getAdjacentCells board cell

flaggedAdjacentCells :: Board -> Cell -> [Cell]
flaggedAdjacentCells board cell = filter (\x -> flagged x) $ getAdjacentCells board cell

getRevealedCells :: Board -> [Cell]
getRevealedCells board = filter (\x -> revealed x) (getAllCells board)

getUnrevealedCells :: Board -> [Cell]
getUnrevealedCells board = filter (\x -> not $ revealed x) (getAllCells board)
