module Minesweeper.Solver (
	autoMove
) where

import Minesweeper.Cell
import Minesweeper.Board
import Minesweeper.Difficulty
import Minesweeper.Game

autoMove :: Board -> Difficulty -> Board
autoMove board difficulty = simpleCountHiddenFlaggedList board (revealedCoords)
  where revealedCoords = getRevealedCoords board

-- Solver can only use revealed tiles
getRevealedCoords :: Board -> [(Int, Int)]
getRevealedCoords board = filtered
  where
    filtered = filter (\x -> revealed (getCell board x)) allCellCoords
    allCellCoords = allPossibleBoardPositions (length (board!!0)) (length board)

-- Simple solver than counts adjacent hidden tiles and 
simpleCountHiddenFlaggedList :: Board -> [(Int, Int)] -> Board
simpleCountHiddenFlaggedList board [] = board
simpleCountHiddenFlaggedList board (coord:coords) = board''
  where
    board'' = simpleCountHiddenFlagged board' coord
    board' = simpleCountHiddenFlaggedList board coords

simpleCountHiddenFlagged :: Board -> (Int, Int) -> Board
simpleCountHiddenFlagged board coord = newBoard
  where 
    newBoard = flagMultiple board coords
    coords = if nahf == (adjacentMines (getCell board coord)) then filterAdjacentCoordsHiddenOrFlagged board coord else []
    nahf = countNumberAdjacentHiddenOrFlagged board coord

flagMultiple :: Board -> [(Int, Int)] -> Board
flagMultiple board [] = board
flagMultiple board (coord:coords) = board''
  where
    board'' = playTurn "flag" board' coord
    board' = flagMultiple board coords

countNumberAdjacentHiddenOrFlagged :: Board -> (Int, Int) -> Int
countNumberAdjacentHiddenOrFlagged board coord = count
  where
    count = sum (map (\x -> if ((not (revealed x)) || (flagged x)) then 1 else 0) adjacentCells)
    adjacentCells = getCells board adjacentCellsCoords
    adjacentCellsCoords = listOfAdjacentTiles coord (length (board!!0)) (length board)

filterAdjacentCoordsHiddenOrFlagged :: Board -> (Int, Int) -> [(Int , Int)]
filterAdjacentCoordsHiddenOrFlagged board coord = filtered
  where
    filtered = filter (\x -> (not (revealed (getCell board x))) || (flagged (getCell board x))) adjacentCellsCoords
    adjacentCellsCoords = listOfAdjacentTiles coord (length (board!!0)) (length board)

