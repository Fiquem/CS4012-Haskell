CS4012 Topics in Functional Programming - Minesweeper Assignment
==============

Our aim was to implement Minesweeper in Haskell, using WX to build the UI, and to create an auto-solver for our implementation.

### To run CLI Minesweeper:
To run the minesweeper command line interface do:
```
ghci
:l Minesweeper/Minesweeper
mainCLI
```

### To play CLI Minesweeper:
There are four simple commands for the command line interface:

```
uncover x y
flag x y
unflag x y
playMove
```