module Minesweeper.Board (
  Board,
  generateRandomBoard,
  showBoard,
  showTrueBoard,
  width,
  height,
  getCell,
  getAllCells,
  getAdjacentCells,
  replaceCell
) where

import Data.List
import System.Random
import Minesweeper.Difficulty
import Minesweeper.Cell

type Board = [[Cell]]
type Width = Int
type Height = Int

--
-- Display Board Function
--
showBoard :: Board -> String
showBoard b = intercalate "\n" (map concat $ map (map show) b)

showTrueBoard :: Board -> String
showTrueBoard b = intercalate "\n" (map concat $ map (map showTrueCell) b)

width :: Board -> Width
width board = length (board!!0)

height :: Board -> Height
height board = length board

--
-- Access Board Cell Functions
--
getCell :: Board -> (Int, Int) -> Cell
getCell board (x, y) = board!!(x-1)!!(y-1)

getCells :: Board -> [(Int, Int)] -> [Cell]
getCells board [] = []
getCells board (coord:coords) = (getCell board coord) : getCells board coords

getAllCells :: Board -> [Cell]
getAllCells board = concat board

getAdjacentCells :: Board -> Cell -> [Cell]
getAdjacentCells board cell = cells
  where
    cells = getCells board coords
    coords = listOfAdjacentTiles (cellCoords cell) (width board) (height board)

--
-- Board Replace Functions
--
replaceCell :: Board -> (Int, Int) -> Cell -> Board
replaceCell oldBoard (x, y) newCell = newBoard
  where
    newBoard = bx ++ [newRow] ++ bys 
    (bx, _:bys) = splitAt (x-1) oldBoard
    newRow = replaceCellAtPosition (oldBoard!!(x-1)) newCell y

replaceCellAtPosition :: [Cell] -> Cell -> Int -> [Cell]
replaceCellAtPosition oldList cell position = rx ++ [cell] ++ rys
    where (rx, _:rys) = splitAt (position-1) oldList

-- Generate Board Functions
generateRandomBoard :: StdGen -> Difficulty -> Board
generateRandomBoard gen difficulty = addCoordinatesToBoard $ calculateAdjacency minedBoard
  where 
    minedBoard = placeMines gen difficulty emptyBoard
    emptyBoard = createBoardOfCells (cols difficulty) (rows difficulty)

createBoardOfCells :: Width -> Height -> Board
createBoardOfCells width height = replicate height $ take width $ cycle [createCell]

-- Coords
addCoordinatesToBoard :: Board -> Board
addCoordinatesToBoard board = addCoordinatesToCellPositions board boardPositions
  where boardPositions = allPossibleBoardPositions (width board) (height board)

addCoordinatesToCellPositions :: Board -> [(Int, Int)] -> Board
addCoordinatesToCellPositions board [] = board
addCoordinatesToCellPositions board (coord:coords) = board''
  where
    board'' = replaceCell board' coord newCell
    newCell = currentCell { cellCoords = coord }
    currentCell = getCell board' coord
    board' = addCoordinatesToCellPositions board coords

-- Adjacent Mines
calculateAdjacency :: Board -> Board
calculateAdjacency board = calculateCellsAdjacency board allPositions
  where
    allPositions = allPossibleBoardPositions (width board) (height board)

calculateCellsAdjacency :: Board -> [(Int, Int)] -> Board
calculateCellsAdjacency board [] = board
calculateCellsAdjacency board (coord:coords) = newBoard
  where
    newBoard = replaceCell modifiedBoard coord newCell
    newCell = currentCell { adjacentMines = cellAdjacency }
    cellAdjacency = calculateCellAdjacency modifiedBoard coord
    currentCell = getCell modifiedBoard coord
    modifiedBoard = calculateCellsAdjacency board coords

calculateCellAdjacency :: Board -> (Int, Int) -> Int
calculateCellAdjacency board coord = sum (map (\x -> if hasMine x then 1 else 0) listAdjacentCells)
  where
    listAdjacentCells = getCells board (listOfAdjacentTiles coord (width board) (height board))

listOfAdjacentTiles :: (Int, Int) -> Width -> Height -> [(Int, Int)]
listOfAdjacentTiles (x, y) width height = filteredList'
  where
    filteredList' = filter (\(x, y) -> x <= height && y <= width) filteredList  -- filter right and bottom sides
    filteredList = filter (\(x, y) -> x > 0 && y > 0) all  -- filter top and left sides
    all = [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y+1), (x+1, y+1), (x+1, y), (x+1, y-1), (x, y-1)]

-- Generate random mine position coordinates
placeMines :: StdGen -> Difficulty -> Board -> Board
placeMines gen difficulty board = placeMinesAtCoordinates board mineCoordinates 
  where
    mineCoordinates = getRandomMinePositions gen possiblePositions (mines difficulty)
    possiblePositions = allPossibleBoardPositions (cols difficulty) (rows difficulty)

placeMinesAtCoordinates :: Board -> [(Int, Int)] -> Board
placeMinesAtCoordinates board [] = board
placeMinesAtCoordinates board (coord:coords) = newBoard
  where
    newBoard = replaceCell modifiedBoard coord newCell 
    newCell = createCell { hasMine = True }
    modifiedBoard = placeMinesAtCoordinates board coords

-- choose mine positions by taking first X number of coordinates off randomly shuffled list of all possible coordinates   
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
