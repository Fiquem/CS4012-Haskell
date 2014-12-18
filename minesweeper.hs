
import System.Random
import Data.Array.IArray
import Data.Char

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

initBoard :: (Int,Int) -> [[Char]]
initBoard (0,_) = []
initBoard (x,y) = (initRow y):(initBoard ((x-1),y))
--

-- User chooses tile to uncover
uncoverTileRow :: [Char] -> Int -> [Char]
uncoverTileRow [] _ = []
uncoverTileRow (rowx:rowxs) 1 = revealed:rowxs
uncoverTileRow (rowx:rowxs) x = rowx:(uncoverTileRow rowxs (x-1))

uncoverTile :: [[Char]] -> (Int,Int) -> [[Char]]
uncoverTile [] (_,_) = []
uncoverTile (boardx:boardxs) (1,y) = (uncoverTileRow boardx y):boardxs
uncoverTile (boardx:boardxs) (x,y) = boardx:(uncoverTile boardxs ((x-1),y))
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

-- Game board with number of adjacent mines
makeStr :: Char -> Int -> [Char]
makeStr _ 0 = []
makeStr x 1 = [x]
makeStr x n = x:(makeStr x (n-1))

sumAdj :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char
sumAdj a b c d e f g h
	| sum == 8*zero				= '0'
	| sum == (7*zero)+one		= '1'
	| sum == (6*zero)+(2*one)	= '2'
	| sum == (5*zero)+(3*one)	= '3'
	| sum == (4*zero)+(4*one)	= '4'
	| sum == (3*zero)+(5*one)	= '5'
	| sum == (2*zero)+(6*one)	= '6'
	| sum == (1*zero)+(7*one)	= '7'
	| otherwise					= '8'
	where 
	sum = ord a + ord b + ord c + ord d + ord e + ord f + ord g + ord g
	zero = ord '_'
	one = ord 'o'

computeNumbersRows :: [Char] -> [Char] -> [Char] -> Int -> [Char]
computeNumbersRows _ _ _ 0 = []
computeNumbersRows (a0:a1:as) (b0:b1:bs) (c0:c1:cs) 1 = [sumAdj a0 a1 '_' b0 '_' c0 c1 '_']
computeNumbersRows (a0:a1:as) (_:'o':bs) (c0:c1:cs) y = 
	'o':(computeNumbersRows (a1:as) ('o':bs) (c1:cs) (y-1))
computeNumbersRows (a0:a1:a2:as) (b0:b1:b2:bs) (c0:c1:c2:cs) y = 
	(sumAdj a0 a1 a2 b0 b1 c0 c1 c2):(computeNumbersRows (a1:as) (b1:bs) (c1:cs) (y-1))

computeNumbersRowsFirst :: [Char] -> [Char] -> [Char] -> Int -> [Char]
computeNumbersRowsFirst _ _ _ 0 = []
computeNumbersRowsFirst (a:as) (b:bs) (c:cs) 1 = [sumAdj '_' a '_' '_' '_' '_' c '_']
computeNumbersRowsFirst (a0:as) ('o':bs) (c0:cs) y = 
	'o':(computeNumbersRows (a0:as) ('o':bs) (c0:cs) (y-1))
computeNumbersRowsFirst (a0:a1:as) (b0:b1:bs) (c0:c1:cs) y = 
	(sumAdj '_' a0 a1 '_' b1 '_' c0 c1):(computeNumbersRows (a0:a1:as) (b0:b1:bs) (c0:c1:cs) (y-1))

computeNumbers :: [[Char]] -> (Int,Int) -> [[Char]]
computeNumbers _ (0,_) = []
computeNumbers (b0:b1:bs) (1,y) = [computeNumbersRowsFirst b0 b1 (makeStr '_' y) y]
computeNumbers (b0:b1:b2:bs) (x,y) = (computeNumbersRowsFirst b0 b1 b2 y):(computeNumbers (b1:b2:bs) ((x-1),y))

computeNumbersFirst :: [[Char]] -> (Int,Int) -> [[Char]]
computeNumbersFirst board (0,_) = []
computeNumbersFirst (b:bs) (1,y) = [computeNumbersRowsFirst (makeStr '_' y) b (makeStr '_' y) y]
computeNumbersFirst (b0:b1:bs) (x,y) = 
	(computeNumbersRowsFirst (makeStr '_' y) b0 b1 y):(computeNumbers (b0:b1:bs) ((x-1),y))
--

main :: IO()
main = do
	g <- newStdGen -- so
	let hiddenBoard = initBoard (5,5)
	let minesBoard = convertToField 
		[[(listArray ((0,0),(4,4)) ( myRands g ) :: Array (Int,Int) Int) -- so
		! (x, y) | x <- [0..4]] | y <- [0..4]] -- so
	--let numbersBoard = computeNumbersFirst minesBoard (5,5)
	let revealedBoard = uncoverTile hiddenBoard (1,1)
	let currentBoard = showCurrentBoard minesBoard revealedBoard
	-- putStrLn $ printArray img
	putStrLn $ unlines $ minesBoard
	putStrLn $ unlines $ hiddenBoard
	putStrLn $ unlines $ revealedBoard
	putStrLn $ unlines $ currentBoard
	--putStrLn $ unlines $ numbersBoard