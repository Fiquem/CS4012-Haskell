{-
    OK, so GHCi and Windows and WXHaskell don't work nicely together
    so this is module Main
    I compile from root directory (above the current one): "ghc Minesweeper/Graphics.hs"
    and I run by "Minesweeper/Graphics.exe"
-}

module Main where

import Data.Char
import System.Random
import Minesweeper.Game
import Minesweeper.Difficulty
import Minesweeper.Board
import Minesweeper.Cell
import Minesweeper.Solver
import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Row)

main :: IO ()
main
  = start gui

gui :: IO ()
gui = do 
    f <- frame [text := "WELCOME TO THE DANGER ZONE", position := pt 100 100] 

    --Buttons
    begin   <- button f [text := "Beginner", on command := (close f >> letsStartTheGameShallWe beginner)]
    inter   <- button f [text := "Intermediate", on command := (close f >> letsStartTheGameShallWe intermediate)]
    expert  <- button f [text := "Expert", on command := (close f >> letsStartTheGameShallWe expert)]
    quit    <- button f [text := "Quit", on command := close f]

    -- specify layout
    set f [layout := column 5 
                [hfill $ hrule 5, fill $ margin 10 $ column 1
                    [floatCentre $ widget begin
                    ,floatCentre $ widget inter
                    ,floatCentre $ widget expert
                    ,floatCentre $ widget quit
                    ]
                ]
                , clientSize := sz 500 400
            ]
    return()
        
letsStartTheGameShallWe :: Difficulty -> IO()
letsStartTheGameShallWe difficulty = do
    gen <- getStdGen -- random number generator seed
    let board = generateRandomBoard gen difficulty
    
    playingTheGame board difficulty "uncover" "flag" "unflag"

    
winlose :: String -> IO()
winlose txt = do
    f <- frame [text := txt, position := pt 100 100]
    b <- button f [text := "Play Again?", on command := (close f >> gui)]
    q <- button f [text := "Quit", on command := close f]
    set f [layout := column 5 $ [floatCentre $ widget b, floatCentre $ widget q], clientSize := sz 600 100]

playingTheGame :: Board -> Difficulty -> String -> String -> String -> IO()
playingTheGame board difficulty txtA txtB txtC = do

    --let fullBoard = showCurrentBoard board revealedBoard
    f <- frame [text := ("Minesweeper: " ++ txtA), position := pt 100 100]
    
    p <- panel f []
     
    --Extra Buttons
    changemode <- button f [text := txtB, on command := (close f >> playingTheGame board difficulty txtB txtA txtC)]
    change2 <- button f [text := txtC, on command := (close f >> playingTheGame board difficulty txtC txtA txtB)]
    
    -- solver currently unavailable
    autoplay <- button f [text := "AutoPlay", on command := (close f >> checkResult (autoMove board difficulty) difficulty)]
    
    --Build Board
    buttons <- buildboard f p txtA difficulty board
    let widgets = buttons2widgets (rows difficulty) buttons
    
    -- specify layout
    set p
            [layout := margin 0 $ column 0 $
                widgets
            ]
            
    set f [layout := column 0 $
                [floatCentre $ widget p
                , (floatBottom $ row 3 [widget changemode, widget change2, widget autoplay])
                ]   
                ,clientSize := sz 500 400
            ]
            
    return()

buildboard :: Frame () -> Panel () -> String -> Difficulty -> Board -> IO[[Button ()]]
buildboard f p txt difficulty board = mapM (buildrow f p txt difficulty board) (zip [1..] board)
    
buildrow :: Frame () -> Panel () -> String -> Difficulty -> Board ->(Int,[Cell]) -> IO[Button ()]
buildrow f p txt difficulty board (rownumber,row) = mapM (getButton f p txt difficulty board rownumber) (zip [1..] row)

getButton :: Frame () -> Panel () -> String -> Difficulty -> Board -> Int -> (Int,Cell) -> IO(Button ())
getButton f p txt difficulty board rownumber (colnumber,cell) = 
    smallButton p [
                    text := show cell
                    ,on command := (close f >> checkResult (playTurn txt board (rownumber, colnumber)) difficulty)
                    ]
                    
checkResult :: Board -> Difficulty -> IO ()
checkResult board difficulty= 
    if isWin board then winlose "A WINNER IS YOU"
                   else if isLose board then winlose "You have shamed us all"
                   else playingTheGame board difficulty "uncover" "flag" "unflag"

--"map (map widget) buttons" didn't work so needed a function to do it
buttons2widgets :: Int -> [[Button ()]] -> [Layout]
buttons2widgets _ [] = []
buttons2widgets rowwidth (x:xs) = row rowwidth (map widget x) : (buttons2widgets rowwidth xs)



