module Minesweeper.Board (
  generateRandomBoard,
  showBoard,
  getCell
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

-- TODO
getCell :: Board -> (Int, Int) -> Cell
getCell board (x, y) = createCell

generateRandomBoard :: StdGen -> Difficulty -> Board
generateRandomBoard gen difficulty = calculateAdjacency minedBoard
  where 
    minedBoard = placeMines gen difficulty emptyBoard
    emptyBoard = createBoardOfCells (rows difficulty) (cols difficulty)

placeMines :: StdGen -> Difficulty -> Board -> Board
placeMines gen difficulty board = placeMinesAtCoordinates mineCoordinates board
  where
    mineCoordinates = getRandomMinePositions gen possiblePositions (mines difficulty)
    possiblePositions = allPossibleBoardPositions (rows difficulty) (cols difficulty)

-- TODO
calculateAdjacency :: Board -> Board
calculateAdjacency board = board

covertBoardToNumbers :: Board -> [[Int]]
covertBoardToNumbers board = map (map (\x -> if hasMine x then 1 else 0)) board

createBoardOfCells :: Width -> Height -> Board
createBoardOfCells width height = replicate height $ take width $ cycle [createCell]



-- TODO
placeMinesAtCoordinates :: [(Int, Int)] -> Board -> Board
placeMinesAtCoordinates coords board = board

-- Generate random mine position coordinates
getRandomMinePositions :: StdGen -> [(Int, Int)] -> Int -> [(Int, Int)]
getRandomMinePositions gen possiblePositions numMines = take numMines $ randPerm gen possiblePositions

allPossibleBoardPositions :: Height -> Width -> [(Int, Int)]
allPossibleBoardPositions 0 _ = []
allPossibleBoardPositions rowNum rowLen = allPossibleBoardPositions (rowNum-1) rowLen ++ zip (replicate rowLen rowNum) (cycle [1..rowLen])

-- Randomly shuffles a list
randPerm :: StdGen -> [(Int, Int)] -> [(Int, Int)]
randPerm _ []   = []
randPerm gen xs = let (n,newGen) = randomR (0, length xs -1) gen
                      front = xs !! n
                  in  front : randPerm newGen (take n xs ++ drop (n+1) xs)