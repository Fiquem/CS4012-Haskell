
import System.Random
import Data.Array.IArray

mine = 'o'
noMine = '_'
hidden = '#'
revealed = ' '


-- stolen from Stack Overflow (so)
-- NOT THE TYPE THO I DID THAT MYSELF ^___^
printArray :: Array (Int,Int) Int -> [Char]
printArray arr =
	unlines [unwords [show (arr ! (x, y)) | x <- [0..5]] | y <- [0..5]]

myRands :: RandomGen g => g -> [Int]
myRands g = randomRs (0,5) g
--

-- Build initial revealed board from given, random input
convertToFieldRow :: [Int] -> [Char]
convertToFieldRow [] = []
convertToFieldRow (x:xs) = 
	case x of
		0 -> mine:(convertToFieldRow xs)
		_ -> noMine:(convertToFieldRow xs)

convertToField :: [[Int]] -> [[Char]]
convertToField [] = []
convertToField (x:xs) = (convertToFieldRow x):(convertToField xs)
--

-- Build initial hidden board
initRow :: Int -> [Char]
initRow 0 = []
initRow x = hidden:(initRow (x-1))

initBoard :: Int -> Int -> [[Char]]
initBoard 0 _ = []
initBoard x y = (initRow y):(initBoard (x-1) y)
--

-- User chooses tile to uncover
uncoverTileRow :: [Char] -> Int -> [Char]
uncoverTileRow [] _ = []
uncoverTileRow (rowx:rowxs) 1 = revealed:rowxs
uncoverTileRow (rowx:rowxs) x = rowx:(uncoverTileRow rowxs (x-1))

uncoverTile :: [[Char]] -> Int -> Int -> [[Char]]
uncoverTile [] _ _ = []
uncoverTile (boardx:boardxs) 1 y = (uncoverTileRow boardx y):boardxs
uncoverTile (boardx:boardxs) x y = boardx:(uncoverTile boardxs (x-1) y)
--

-- Current game state
showCurrentBoardRows :: [Char] -> [Char] -> [Char]
showCurrentBoardRows [] [] = []
showCurrentBoardRows (b:bs) (h:hs) = 
	case h of
		'#' -> h:(showCurrentBoardRows bs hs)
		' ' -> b:(showCurrentBoardRows bs hs)

showCurrentBoard :: [[Char]] -> [[Char]] -> [[Char]]
showCurrentBoard [] [] = []
showCurrentBoard (b:bs) (h:hs) = (showCurrentBoardRows b h):(showCurrentBoard bs hs)
--

main :: IO()
main = do
	g <- newStdGen -- so
	let hiddenBoard = initBoard 5 5
	let board = convertToField 
		[[(listArray ((0,0),(4,4)) ( myRands g ) :: Array (Int,Int) Int) -- so
		! (x, y) | x <- [0..4]] | y <- [0..4]] -- so
	let revealedBoard = uncoverTile hiddenBoard 1 1
	let currentBoard = showCurrentBoard board revealedBoard
	-- putStrLn $ printArray img
	putStrLn $ unlines $ board
	putStrLn $ unlines $ hiddenBoard
	putStrLn $ unlines $ revealedBoard
	putStrLn $ unlines $ currentBoard