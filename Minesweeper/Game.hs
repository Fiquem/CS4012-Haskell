module Minesweeper.Game (
  playTurn,
  isWin,
  isLose
) where

import Minesweeper.Cell
import Minesweeper.Board
import Minesweeper.Difficulty

playTurn :: String -> Board -> (Int, Int) -> Board
playTurn action board coord =
  case action of
    "uncover" ->  uncover board coord
    "flag"    ->  flag board coord
    otherwise ->  unflag board coord

isWin :: Board -> Bool
isWin board = all (\x -> if (revealed x) then (not (hasMine x)) else (hasMine x)) (getAllCells board)

isLose :: Board -> Bool
isLose board = any (\x -> (hasMine x) && (revealed x)) (getAllCells board)

-- sub actions
flag :: Board -> (Int, Int) -> Board
flag board coord = replaceCell board coord newCell
  where newCell = (getCell board coord) { flagged = True }

unflag :: Board -> (Int, Int) -> Board
unflag board coord = replaceCell board coord newCell
  where newCell = (getCell board coord) { flagged = False }

uncover :: Board -> (Int, Int) -> Board
uncover board coord = finalBoard
  where
    finalBoard = if ((numberMines == 0) && (not (revealed oldCell)))
      then propagateUncover replacedBord coord
      else replacedBord
    replacedBord = replaceCell board coord newCell
    numberMines = adjacentMines oldCell
    newCell = oldCell { revealed = True }
    oldCell = getCell board coord

-- uncover helpers
propagateUncover :: Board -> (Int, Int) -> Board
propagateUncover board coord = uncoverListCells board adjacentCells
  where
    adjacentCells = listOfAdjacentTiles coord (length (board!!0)) (length board)

uncoverListCells :: Board -> [(Int, Int)] -> Board
uncoverListCells board [] = board
uncoverListCells board (coord:coords) = newBoard
  where
    newBoard = uncover modifiedBoard coord
    modifiedBoard = uncoverListCells board coords