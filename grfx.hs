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
    
    playingTheGame internalBoard initialBoard difficulty

    
winlose :: String -> IO()
winlose txt = do
    f <- frame [text := txt]
    b <- button f [text := "Play Again?", on command := (close f >> gui)]
    q <- button f [text := "Quit", on command := close f]
    set f [layout := column 5 $ [floatCentre $ widget b, floatCentre $ widget q], clientSize := sz 600 100]

playingTheGame :: InternalBoard -> PlayerBoard -> Difficulty -> IO()
playingTheGame board revealedBoard difficulty = do
    --Boards = [Rows] = [ [Tiles] ] = [ [Char] ]
    let fullBoard = showCurrentBoard board revealedBoard
    f <- frame [text := "Minesweeper"]
    
    -- use text control as logger
    textlog <- textCtrl f [wrap := WrapNone, enabled := False] 
    textCtrlMakeLogActiveTarget textlog
    logMessage "logging enabled" 
    
    p <- panel f []
    
    
    --Extra Buttons
    wincheck <- button f [text := "isWin", on command := logMessage(checkWinCond board revealedBoard)]
    printBoard <- button f [text := "Board", on command := logMessage(unlines $ fullBoard)]
    
    --Build Board
    buttons <- buildboard f p difficulty board revealedBoard fullBoard
    let widgets = buttons2widgets (rows difficulty)buttons
    
    -- specify layout
    set p
            [layout := margin 0 $ column 0 $
                widgets
            ]
            
    set f [layout := column 0 $
                [floatCentre $ widget p
                , floatTop $ widget wincheck
                , floatBottom $ widget printBoard
                ,hfill $ minsize (sz 200 200) $ widget textlog
                ]   
                ,clientSize := sz 500 400
            ]
            
    return()
    
checkWinCond :: InternalBoard -> PlayerBoard -> String
checkWinCond a b = 
    if (isWin a b) then "Win Condition Reached"
    else "Win Condition Not Reached"

buildboard :: Frame () -> Panel () -> Difficulty -> InternalBoard -> PlayerBoard -> PlayerBoard -> IO[[Button ()]]
buildboard f p dif hidden board full= mapM (buildrow f p dif hidden board) (zip [1..] full)
    
buildrow :: Frame () -> Panel () -> Difficulty -> InternalBoard -> PlayerBoard -> (Int,Row) -> IO[Button ()]
buildrow f p dif hidden board (rownumber,row) = mapM (getButton f p dif hidden board rownumber) (zip [1..] row)

getButton :: Frame () -> Panel () -> Difficulty -> InternalBoard -> PlayerBoard -> Int -> (Int,Tile) -> IO(Button ())
getButton f p dif hidden board rownumber(colnumber,tile) = 
    smallButton p [
                    text := [tile]
                    , size := sz 30 30
                    ,on command :=     
                                    let newBoard = playTurn "uncover" hidden board dif (rownumber,colnumber)
                                    in if isWin hidden newBoard then close f >> winlose "A WINNER IS YOU"
                                                                else if isLose hidden newBoard then close f >> winlose "You have shamed us all"
                                                                else close f >> playingTheGame hidden newBoard dif
                    --, on command :=  checkForWin f hidden (playTurn "uncover" hidden board dif (rownumber,colnumber)) dif
                    --, on clickRight := (close f >> checkForWin f hidden (playTurn "flag" hidden board dif (rownumber,colnumber) dif)
                    ]




--"map (map widget) buttons" didn't work so needed a function to do it
buttons2widgets :: Int -> [[Button ()]] -> [Layout]
buttons2widgets _ [] = []
buttons2widgets rowwidth (x:xs) = row rowwidth (map widget x) : (buttons2widgets rowwidth xs)



