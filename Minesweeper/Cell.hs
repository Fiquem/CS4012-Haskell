module Minesweeper.Cell where

data Cell = Cell { hasMine :: Bool
                 , flagged :: Bool
                 , revealed :: Bool
                 , adjacentMines :: Int
                 }

instance Show Cell where
  show cell
    | flagged cell        = " F "
    | not (revealed cell) = " _ "
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
                  }