module Main where

import Data.Char
import System.Random
import Minesweeper
import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Row)

main :: IO ()
main
  = start gui

gui :: IO ()
gui = do 
    f <- frame [text := "WELCOME TO THE DANGER ZONE"] 

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
    let initialBoard = initBoard (rows difficulty,cols difficulty)
    let minesBoard = generateRandomBoard gen difficulty
    let internalBoard = computeNumbersFirst minesBoard (rows difficulty,cols difficulty)
    
    playingTheGame internalBoard initialBoard difficulty "uncover" "flag" "unflag"

    
winlose :: String -> IO()
winlose txt = do
    f <- frame [text := txt]
    b <- button f [text := "Play Again?", on command := (close f >> gui)]
    q <- button f [text := "Quit", on command := close f]
    set f [layout := column 5 $ [floatCentre $ widget b, floatCentre $ widget q], clientSize := sz 600 100]

playingTheGame :: InternalBoard -> PlayerBoard -> Difficulty -> String -> String -> String -> IO()
playingTheGame board revealedBoard difficulty txtA txtB txtC = do
    --Boards = [Rows] = [ [Tiles] ] = [ [Char] ]
    let fullBoard = showCurrentBoard board revealedBoard
    f <- frame [text := ("Minesweeper: " ++ txtA)]
    
    p <- panel f []
     
    --Extra Buttons
    changemode <- button f [text := txtB, on command := (close f >> playingTheGame board revealedBoard difficulty txtB txtA txtC)]
    change2 <- button f [text := txtC, on command := (close f >> playingTheGame board revealedBoard difficulty txtC txtA txtB)]
    autoplay <- button f [text := "AutoPlay", on command := (close f >> checkResult board (playMove board revealedBoard difficulty) difficulty)]
    
    --Build Board
    buttons <- buildboard f p txtA difficulty board revealedBoard fullBoard
    let widgets = buttons2widgets (rows difficulty)buttons
    
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

buildboard :: Frame () -> Panel () -> String -> Difficulty -> InternalBoard -> PlayerBoard -> PlayerBoard -> IO[[Button ()]]
buildboard f p txt dif hidden board full= mapM (buildrow f p txt dif hidden board) (zip [1..] full)
    
buildrow :: Frame () -> Panel () -> String -> Difficulty -> InternalBoard -> PlayerBoard -> (Int,Row) -> IO[Button ()]
buildrow f p txt dif hidden board (rownumber,row) = mapM (getButton f p txt dif hidden board rownumber) (zip [1..] row)

getButton :: Frame () -> Panel () -> String -> Difficulty -> InternalBoard -> PlayerBoard -> Int -> (Int,Tile) -> IO(Button ())
getButton f p txt dif hidden board rownumber(colnumber,tile) = 
    smallButton p [
                    text := [tile]
                    , size := sz 30 30
                    ,on command := (close f >> checkResult hidden (playTurn txt hidden board dif (rownumber, colnumber)) dif)
                    ]
                    
checkResult :: InternalBoard -> PlayerBoard -> Difficulty -> IO ()
checkResult board revealedBoard difficulty = 
    if isWin board revealedBoard then winlose "A WINNER IS YOU"
                             else if isLose board revealedBoard then winlose "You have shamed us all"
                             else playingTheGame board revealedBoard difficulty "uncover" "flag" "unflag"

--"map (map widget) buttons" didn't work so needed a function to do it
buttons2widgets :: Int -> [[Button ()]] -> [Layout]
buttons2widgets _ [] = []
buttons2widgets rowwidth (x:xs) = row rowwidth (map widget x) : (buttons2widgets rowwidth xs)



