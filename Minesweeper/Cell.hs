module Minesweeper.Cell where

data Cell = Cell { hasMine :: Bool
                 , flagged :: Bool
                 , revealed :: Bool
                 , adjacentMines :: Int
                 , cellCoords :: (Int, Int)
                 }

instance Show Cell where
  show cell
    | flagged cell        = " F "
    | not (revealed cell) = " # "
    | hasMine cell        = " * "
    | otherwise           = " " ++ show (adjacentMines cell) ++ " "

showTrueCell :: Cell -> String
showTrueCell cell =
  if hasMine cell
    then " * "
    else " " ++ show (adjacentMines cell) ++ " "

createCell :: Cell
createCell = Cell { hasMine = False
                  , flagged = False
                  , revealed = False
                  , adjacentMines = 0
                  , cellCoords = (-1, -1)
                  }