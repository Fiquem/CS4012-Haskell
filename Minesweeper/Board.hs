module Minesweeper.Board (
  Board,
  generateRandomBoard,
  showBoard,
  showTrueBoard,
  getCell,
  replaceCell
) where

import Debug.Trace
import Data.List
import System.Random
import Minesweeper.Difficulty
import Minesweeper.Cell

type Board = [[Cell]]
type Width = Int
type Height = Int

showBoard :: Board -> String
showBoard b = intercalate "\n" (map concat $ map (map show) b)

showTrueBoard :: Board -> String
showTrueBoard b = intercalate "\n" (map concat $ map (map showTrueCell) b)

-- TODO: return Maybe Cell
getCell :: Board -> (Int, Int) -> Cell
getCell board (x, y) = board!!(y-1)!!(x-1)

getCells :: Board -> [(Int, Int)] -> [Cell]
getCells board [] = []
getCells board (coord:coords) = (getCell board coord) : getCells board coords

replaceCell :: Board -> (Int, Int) -> Cell -> Board
replaceCell oldBoard (x, y) newCell = newBoard
  where
    newBoard = bx ++ [newRow] ++ bys 
    (bx, _:bys) = splitAt (y-1) oldBoard
    newRow = replaceCellAtPosition (oldBoard!!(y-1)) newCell x

replaceCellAtPosition :: [Cell] -> Cell -> Int -> [Cell]
replaceCellAtPosition oldList cell position = rx ++ [cell] ++ rys
    where (rx, _:rys) = splitAt (position-1) oldList

generateRandomBoard :: StdGen -> Difficulty -> Board
generateRandomBoard gen difficulty = calculateAdjacency minedBoard (cols difficulty) (rows difficulty)
  where 
    minedBoard = placeMines gen difficulty emptyBoard
    emptyBoard = createBoardOfCells (cols difficulty) (rows difficulty)

placeMines :: StdGen -> Difficulty -> Board -> Board
placeMines gen difficulty board = placeMinesAtCoordinates board mineCoordinates 
  where
    mineCoordinates = getRandomMinePositions gen possiblePositions (mines difficulty)
    possiblePositions = allPossibleBoardPositions (cols difficulty) (rows difficulty)

calculateAdjacency :: Board -> Width -> Height -> Board
calculateAdjacency board width height = calculateCellsAdjacency board allPositions width height
  where
    allPositions = allPossibleBoardPositions width height

calculateCellsAdjacency :: Board -> [(Int, Int)] -> Width -> Height -> Board
calculateCellsAdjacency board [] _ _ = board
calculateCellsAdjacency board (coord:coords) width height = newBoard
  where
    newBoard = replaceCell modifiedBoard coord newCell
    newCell = createCell { hasMine = (hasMine currentCell), adjacentMines = cellAdjacency }
    cellAdjacency = calculateCellAdjacency modifiedBoard coord width height
    currentCell = getCell modifiedBoard coord
    modifiedBoard = calculateCellsAdjacency board coords width height

calculateCellAdjacency :: Board -> (Int, Int) -> Width -> Height -> Int
calculateCellAdjacency board coord width height = sum (map (\x -> if hasMine x then 1 else 0) listAdjacentCells)
  where
    listAdjacentCells = getCells board (listOfAdjacentTiles coord width height)

listOfAdjacentTiles :: (Int, Int) -> Width -> Height -> [(Int, Int)]
listOfAdjacentTiles (x, y) width height = filteredList'
  where
    filteredList' = filter (\(x, y) -> x <= height && y <= width) filteredList  -- filter right and bottom sides
    filteredList = filter (\(x, y) -> x > 0 && y > 0) all  -- filter top and left sides
    all = [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y+1), (x+1, y+1), (x+1, y), (x+1, y-1), (x, y-1)]

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

allPossibleBoardPositions :: Width -> Height -> [(Int, Int)]
allPossibleBoardPositions _ 0 = []
allPossibleBoardPositions width height = allPossibleBoardPositions width (height-1) ++ zip (replicate width height) (cycle [1..width])

-- Randomly shuffles a list
randPerm :: StdGen -> [(Int, Int)] -> [(Int, Int)]
randPerm _ []   = []
randPerm gen xs = let (n, newGen) = randomR (0, length xs -1) gen
                      front = xs !! n
                  in  front : randPerm newGen (take n xs ++ drop (n+1) xs)
