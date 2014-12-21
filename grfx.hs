module Main where

import Graphics.UI.WX

main :: IO ()
main
  = start gui

testcommand :: String
testcommand = "command successful"

gui :: IO ()
gui = do 
    f <- frame [text := "WELCOME TO THE DANGER ZONE"] 
           
    --Widgets
    
    begin   <- button f [text := "Beginner", on command := close f]
    inter   <- button f [text := "Intermediate", on command := close f]
    expert  <- button f [text := "Expert", on command := close f]
    quit    <- button f [text := "Quit", on command := close f]

    -- specify layout
    set f [layout := minsize (sz 500 400) $ column 1
            [hfill $ hrule 5, fill $ margin 10 $ column 1
                [floatCentre $ widget begin
                ,floatCentre $ widget inter
                ,floatCentre $ widget expert
                ,floatCentre $ widget quit]
            ]
        ]