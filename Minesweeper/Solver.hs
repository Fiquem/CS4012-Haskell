module Minesweeper.Solver (
	autoMove
) where

import Minesweeper.Board
import Minesweeper.Difficulty

autoMove :: Board -> Difficulty -> Board
autoMove board difficulty = board

countNumberAdjacentHidden :: Board -> (Int, Int) -> Int
countNumberAdjacentHidden board coord = count
  where
    count = sum (map (\x -> if revealed x then 0 else 1) adjacentCells)
    adjacentCells = getCells adjacentCellsCoords
    adjacentCellsCoords = listOfAdjacentTiles coord (length (board!!0)) (length board)

filterAdjacentCoordsHidden :: Board -> (Int, Int) -> [(Int , Int)]
filterAdjacentCoordsHidden board coord = filtered
  where
    filtered = filter (\x -> not (revealed (getCell board x))) adjacentCellsCoords
    adjacentCellsCoords = listOfAdjacentTiles coord (length (board!!0)) (length board)

