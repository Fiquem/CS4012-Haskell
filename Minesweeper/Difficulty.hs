module Minesweeper.Difficulty where

data Difficulty = Difficulty { rows :: Int
							 , cols :: Int
							 , mines :: Int
							 } deriving (Show)

beginner = Difficulty 9 9 10
intermediate = Difficulty 16 16 40
expert = Difficulty 16 30 99