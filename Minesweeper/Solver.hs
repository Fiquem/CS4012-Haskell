module Minesweeper.Solver (
	autoMove
) where

import Minesweeper.Board
import Minesweeper.Difficulty

autoMove :: Board -> Difficulty -> Board
autoMove board difficulty = board