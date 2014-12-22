module Minesweeper.Board (
  Board,
  generateRandomBoard,
  showBoard,
  getCell,
  replaceCell
) where

import Data.List
import System.Random
import Minesweeper.Difficulty
import Minesweeper.Cell

type Board = [[Cell]]
type Width = Int
type Height = Int

showBoard :: Board -> String
showBoard b = intercalate "\n" (map concat $ map (map show) b)

-- TODO: return Maybe Cell
getCell :: Board -> (Int, Int) -> Cell
getCell board (x, y) = board!!(y-1)!!(x-1)

replaceCell :: Board -> (Int, Int) -> Cell -> Board
replaceCell oldBoard newCell (x, y) = newBoard
  where
    newBoard = bx ++ [newRow] ++ bys 
    (bx, _:bys) = splitAt (y-1) oldBoard
    newRow = replaceCellAtPosition (oldBoard!!(y-1)) newCell x  

replaceCellAtPosition :: [Cell] -> Cell -> Int -> [Cell]
replaceCellAtPosition oldList cell position = rx ++ [cell] ++ rys
    where (rx, _:rys) = splitAt (position-1) oldList

generateRandomBoard :: StdGen -> Difficulty -> Board
generateRandomBoard gen difficulty = calculateAdjacency minedBoard
  where 
    minedBoard = placeMines gen difficulty emptyBoard
    emptyBoard = createBoardOfCells (rows difficulty) (cols difficulty)

placeMines :: StdGen -> Difficulty -> Board -> Board
placeMines gen difficulty board = placeMinesAtCoordinates board mineCoordinates 
  where
    mineCoordinates = getRandomMinePositions gen possiblePositions (mines difficulty)
    possiblePositions = allPossibleBoardPositions (rows difficulty) (cols difficulty)

-- TODO
calculateAdjacency :: Board -> Board
calculateAdjacency board = board


-- Returns board of locations of mines as 1 and no mines as 0
covertBoardToNumbers :: Board -> [[Int]]
covertBoardToNumbers board = map (map (\x -> if hasMine x then 1 else 0)) board

createBoardOfCells :: Width -> Height -> Board
createBoardOfCells width height = replicate height $ take width $ cycle [createCell]

placeMinesAtCoordinates :: Board -> [(Int, Int)] -> Board
placeMinesAtCoordinates board [] = board
placeMinesAtCoordinates board (coord:coords) = newBoard
  where
    newBoard = replaceCell modifiedBoard coord newCell 
    newCell = createCell { hasMine = True }
    modifiedBoard = placeMinesAtCoordinates board coords

-- Generate random mine position coordinates
getRandomMinePositions :: StdGen -> [(Int, Int)] -> Int -> [(Int, Int)]
getRandomMinePositions gen possiblePositions numMines = take numMines $ randPerm gen possiblePositions

allPossibleBoardPositions :: Height -> Width -> [(Int, Int)]
allPossibleBoardPositions 0 _ = []
allPossibleBoardPositions rowNum rowLen = allPossibleBoardPositions (rowNum-1) rowLen ++ zip (replicate rowLen rowNum) (cycle [1..rowLen])

-- Randomly shuffles a list
randPerm :: StdGen -> [(Int, Int)] -> [(Int, Int)]
randPerm _ []   = []
randPerm gen xs = let (n, newGen) = randomR (0, length xs -1) gen
                      front = xs !! n
                  in  front : randPerm newGen (take n xs ++ drop (n+1) xs)
