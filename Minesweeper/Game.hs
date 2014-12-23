module Minesweeper.Game where

import System.Random
import Data.Char
import Data.List.Split

import Minesweeper.Cell
import Minesweeper.Board
import Minesweeper.Difficulty

type GameResult = IO [Char]

uncover :: Board -> (Int, Int) -> Board
uncover board coord = replaceCell board coord newCell
  where newCell = (getCell board coord) { revealed = True }

flag :: Board -> (Int, Int) -> Board
flag board coord = replaceCell board coord newCell
  where newCell = (getCell board coord) { flagged = True }

unflag :: Board -> (Int, Int) -> Board
unflag board coord = replaceCell board coord newCell
  where newCell = (getCell board coord) { flagged = False }

isWin :: Board -> Bool
isWin board = all (\x -> (not (hasMine x)) && (revealed x)) (getAllCells board)

isLose :: Board -> Bool
isLose board = any (\x -> (hasMine x) && (revealed x)) (getAllCells board)

-- Game Loop
playTurn :: String -> Board -> Difficulty -> (Int, Int) -> Board
playTurn action board difficulty coord =
  case action of
    "uncover" ->  uncover board coord
    "flag"    ->  flag board coord
    otherwise ->  unflag board coord

getUserInputAndMakeSureIt'sNotShittyInput :: Difficulty -> IO [[Char]]
getUserInputAndMakeSureIt'sNotShittyInput difficulty = do
  coords <- getLine
  let coords' = splitOn " " coords
  if length coords' == 3 && ((coords' !! 0) :: String) `elem` ["flag", "unflag", "uncover"] && 
    (read (coords' !! 1) :: Int) `elem` [1..(cols difficulty)] && (read (coords' !! 2) :: Int) `elem` [1..(rows difficulty)]
    then return coords'
    else if length coords' == 1 && ((coords' !! 0) :: String) == "playMove"
      then return (coords'++[" 0 0"])
      else do
        putStrLn "Unrecognised input. Try again."
        getUserInputAndMakeSureIt'sNotShittyInput difficulty

playGame :: Board -> Difficulty -> GameResult
playGame board difficulty = do
  -- User prompt
  putStrLn "Enter coordinates to uncover in format: action x y"
  putStrLn "Actions: flag unflag uncover"
  putStrLn "Indexed from 1\n"

  -- User input
  input <- getUserInputAndMakeSureIt'sNotShittyInput difficulty
  let action = (input !! 0)
  let x = read (input !! 2) :: Int
  let y = read (input !! 1) :: Int
  putStrLn ""

  -- Make a move
  let board' = playTurn action board difficulty (x, y)
    --if action == "playMove"
    --  then playMove board difficulty
     -- else playTurn action board difficulty (x, y)

  -- Show game state
  putStrLn "\nCurrent Board"
  putStrLn $ showBoard board'

  -- Detect endgame or repeat
  if (isLose board') then return "LOSE"
  else if (isWin board') then return "WIN"
  else playGame board' difficulty

main :: IO()
main = do
  -- Game setup
  gen <- newStdGen -- random number generator seed
  let difficulty = beginner
  let board = generateRandomBoard gen difficulty

  putStrLn $ showBoard board
  putStrLn $ "\n"
  putStrLn $ showTrueBoard board

  result <- playGame board difficulty
  putStrLn result

