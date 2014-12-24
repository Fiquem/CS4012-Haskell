module Minesweeper.Game (
  playTurn,
  isWin,
  isLose,
  uncover,
  flag,
  applyToCells
) where

import Debug.Trace
import Minesweeper.Cell
import Minesweeper.Board
import Minesweeper.Difficulty

playTurn :: String -> Board -> (Int, Int) -> Board
playTurn action board coord =
  case action of
    "uncover" ->  uncover board (getCell board coord)
    "flag"    ->  flag board (getCell board coord)
    otherwise ->  unflag board (getCell board coord)

isWin :: Board -> Bool
isWin board = all (\x -> if (revealed x) then (not (hasMine x)) else (hasMine x)) (getAllCells board)

isLose :: Board -> Bool
isLose board = any (\x -> (hasMine x) && (revealed x)) (getAllCells board)

-- sub actions
flag :: Board -> Cell -> Board
flag board cell = replaceCell board (cellCoords cell) newCell
  where newCell = cell { flagged = True }

unflag :: Board -> Cell -> Board
unflag board cell = replaceCell board (cellCoords cell) newCell
  where newCell = cell { flagged = False }

uncover :: Board -> Cell -> Board
uncover board cell = finalBoard
  where
    finalBoard = if ((numberMines == 0) && (not (revealed cell)))
      then propagateUncover replacedBord cell
      else replacedBord
    replacedBord = replaceCell board (cellCoords cell) newCell
    numberMines = adjacentMines cell
    newCell = cell { revealed = True }

applyToCells :: Board -> (Board -> Cell -> Board) -> [Cell] -> Board
applyToCells board _ [] = board
applyToCells board f (x:xs) = f board' x
  where board' = applyToCells board f xs

-- uncover helpers
propagateUncover :: Board -> Cell -> Board
propagateUncover board cell = applyToCells board uncover (getAdjacentCells board cell)
