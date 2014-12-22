module Main where

import Data.Char
import System.Random
import Minesweeper
import Graphics.UI.WX

main :: IO ()
main
  = start gui

testcommand :: String
testcommand = "command successful"

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
    
playingTheGame ::InternalBoard -> PlayerBoard -> Difficulty -> IO()
playingTheGame board maskBoard difficulty = do
    --Boards = [Rows] = [ [Tiles] ] = [ [Char] ]
    
    f <- frame [text := "Sweep Them Mines Yo"]
    let revealedBoard = showCurrentBoard board maskBoard
    
    --Build Board
    buttons <- buildboard f difficulty board revealedBoard
    let widgets = buttons2widgets (rows difficulty)buttons
    
    --Buttons
    --boardButton     <- button f [text := "Show Board", on command := (logMessage $ unlines $ revealedBoard)]
    --quit            <- button f [text := "Quit", on command := close f]
    -- specify layout
    set f
            [layout := column 0 $
                widgets
                , clientSize := sz 500 400
            ]
            
    return()

buildboard :: Frame () -> Difficulty -> InternalBoard -> PlayerBoard -> IO[[Button ()]]
buildboard f dif hidden board= mapM (buildrow f dif hidden board) (zip [1..] board)
    
buildrow :: Frame () -> Difficulty -> InternalBoard -> PlayerBoard -> (Int,Row) -> IO[Button ()]
buildrow f dif hidden board (rownumber,row) = mapM (getButton f dif hidden board rownumber) (zip [1..] row)

getButton :: Frame () -> Difficulty -> InternalBoard -> PlayerBoard -> Int -> (Int,Tile) -> IO(Button ())
getButton f dif hidden board rownumber(colnumber,tile) = smallButton f [
                                                    text := [tile]
                                                    , on command := ( close f >> playingTheGame hidden (playTurn "uncover" hidden board dif (rownumber,colnumber)) dif)
                                                    --, on clickRight := (playingTheGame f hidden (playTurn "flag" hidden board dif (rownumber,colnumber) dif)
                                                    ]



--"map (map widget) buttons" didn't work so needed a function to do it
buttons2widgets :: Int -> [[Button ()]] -> [Layout]
buttons2widgets _ [] = []
buttons2widgets rowwidth (x:xs) = row rowwidth (map widget x) : (buttons2widgets rowwidth xs)



