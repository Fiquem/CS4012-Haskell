
import System.Random
import Data.Array.IArray
import Data.Char
import Data.List.Split

mine = 'o'
noMine = '_'
hidden = '#'
revealed = ' '
flagged = 'F'


-- stolen from Stack Overflow (so)
-- NOT THE TYPE THO I DID THAT MYSELF ^___^
printArray :: Array (Int,Int) Int -> [Char]
printArray arr =
	unlines [unwords [show (arr ! (x, y)) | x <- [0..6]] | y <- [0..6]]

myRands :: RandomGen g => g -> [Int]
myRands g = randomRs (0,15) g
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
uncoverTileRow (rowx:rowxs) 1 = 
	case rowx of
		'F' -> rowx:rowxs
		_	-> revealed:rowxs
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
		'F' -> h:(showCurrentBoardRows bs hs)
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
	| sum == 8*zero				= '_'
	| sum == (7*zero)+one		= '1'
	| sum == (6*zero)+(2*one)	= '2'
	| sum == (5*zero)+(3*one)	= '3'
	| sum == (4*zero)+(4*one)	= '4'
	| sum == (3*zero)+(5*one)	= '5'
	| sum == (2*zero)+(6*one)	= '6'
	| sum == (1*zero)+(7*one)	= '7'
	| otherwise					= '8'
	where 
	sum = ord a + ord b + ord c + ord d + ord e + ord f + ord g + ord h
	zero = ord '_'
	one = ord 'o'

--	a b c
--	d ? e
--	f g h

computeNumbersRows :: [Char] -> [Char] -> [Char] -> Int -> [Char]
computeNumbersRows _ _ _ 0 = []
computeNumbersRows _ (_:'o':_) _ 1 = ['o']
computeNumbersRows (a0:a1:as) (b0:b1:bs) (c0:c1:cs) 1 = [sumAdj a0 a1 '_' b0 '_' c0 c1 '_']
computeNumbersRows (a0:a1:as) (_:'o':bs) (c0:c1:cs) y = 
	'o':(computeNumbersRows (a1:as) ('o':bs) (c1:cs) (y-1))
computeNumbersRows (a0:a1:a2:as) (b0:b1:b2:bs) (c0:c1:c2:cs) y = 
	(sumAdj a0 a1 a2 b0 b2 c0 c1 c2):(computeNumbersRows (a1:a2:as) (b1:b2:bs) (c1:c2:cs) (y-1))
computeNumbersRows a b c y = " "++[intToDigit y]++" "++a++" "++b++" "++c

computeNumbersRowsFirst :: [Char] -> [Char] -> [Char] -> Int -> [Char]
computeNumbersRowsFirst _ _ _ 0 = []
computeNumbersRowsFirst _ ('o':_) _ 1 = ['o']
computeNumbersRowsFirst (a:as) (b:bs) (c:cs) 1 = [sumAdj '_' a '_' '_' '_' '_' c '_']
computeNumbersRowsFirst (a0:as) ('o':bs) (c0:cs) y = 
	'o':(computeNumbersRows (a0:as) ('o':bs) (c0:cs) (y-1))
computeNumbersRowsFirst (a0:a1:as) (b0:b1:bs) (c0:c1:cs) y = 
	(sumAdj '_' a0 a1 '_' b1 '_' c0 c1):(computeNumbersRows (a0:a1:as) (b0:b1:bs) (c0:c1:cs) (y-1))
computeNumbersRowsFirst a b c y = " "++[intToDigit y]++" "++a++" "++b++" "++c

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

-- Uncover all tiles adjacent to non-numbered tiles
findTileRow :: [Char] -> Int -> Char
findTileRow (rowx:rowxs) 1 = rowx
findTileRow (rowx:rowxs) x = findTileRow rowxs (x-1)

findTile :: [[Char]] -> (Int,Int) -> Char
findTile (boardx:boardxs) (1,y) = findTileRow boardx y
findTile (boardx:boardxs) (x,y) = findTile boardxs ((x-1),y)

buncoverTile :: [[Char]] -> [[Char]] -> (Int,Int) -> (Int,Int) -> [[Char]]
buncoverTile board hidden (a,b) (x,y) = do
	revealedBoard <- [uncoverTile hidden (x,y)]
	if findTile hidden (x,y) == '#' && findTile board (x,y) == '_'
		then
			if x == 1 && y == 1	then do
				revealedBoard' <- [buncoverTile board revealedBoard (a,b) (x,(y+1))]
				revealedBoard'' <- [buncoverTile board revealedBoard' (a,b) ((x+1),(y+1))]
				buncoverTile board revealedBoard'' (a,b) ((x+1),y)
			else
			if x == 1 && y == b	then do
				revealedBoard' <- [buncoverTile board revealedBoard (a,b) (x,(y-1))]
				revealedBoard'' <- [buncoverTile board revealedBoard' (a,b) ((x+1),(y-1))]
				buncoverTile board revealedBoard'' (a,b) ((x+1),y)
			else
			if x == 1 then do
				revealedBoard' <- [buncoverTile board revealedBoard (a,b) (x,(y+1))]
				revealedBoard'' <- [buncoverTile board revealedBoard' (a,b) ((x+1),(y+1))]
				revealedBoard''' <- [buncoverTile board revealedBoard'' (a,b) ((x+1),y)]
				revealedBoard'''' <- [buncoverTile board revealedBoard''' (a,b) ((x+1),(y-1))]
				buncoverTile board revealedBoard'''' (a,b) (x,(y-1))
			-- I'm actually really sorry about this but I don't know what else to do
			else
			if x == a && y == 1 then do
				revealedBoard' <- [buncoverTile board revealedBoard (a,b) (x,(y+1))]
				revealedBoard'' <- [buncoverTile board revealedBoard' (a,b) ((x-1),(y+1))]
				buncoverTile board revealedBoard'' (a,b) ((x-1),y)
			else
			if x == a && y == b	then do
				revealedBoard' <- [buncoverTile board revealedBoard (a,b) ((x-1),y)]
				revealedBoard'' <- [buncoverTile board revealedBoard' (a,b) ((x-1),(y-1))]
				buncoverTile board revealedBoard'' (a,b) (x,(y-1))
			else
			if x == a then do
				revealedBoard' <- [buncoverTile board revealedBoard (a,b) (x,(y-1))]
				revealedBoard'' <- [buncoverTile board revealedBoard' (a,b) ((x-1),(y-1))]
				revealedBoard''' <- [buncoverTile board revealedBoard'' (a,b) ((x-1),y)]
				revealedBoard'''' <- [buncoverTile board revealedBoard''' (a,b) ((x-1),(y+1))]
				buncoverTile board revealedBoard'''' (a,b) (x,(y+1))
			else
			if y == 1 then do
				revealedBoard' <- [buncoverTile board revealedBoard (a,b) ((x-1),y)]
				revealedBoard'' <- [buncoverTile board revealedBoard' (a,b) ((x-1),(y+1))]
				revealedBoard''' <- [buncoverTile board revealedBoard'' (a,b) (x,(y+1))]
				revealedBoard'''' <- [buncoverTile board revealedBoard''' (a,b) ((x+1),(y+1))]
				buncoverTile board revealedBoard'''' (a,b) ((x+1),y)
			else
			if y == b then do
				revealedBoard' <- [buncoverTile board revealedBoard (a,b) ((x+1),y)]
				revealedBoard'' <- [buncoverTile board revealedBoard' (a,b) ((x+1),(y-1))]
				revealedBoard''' <- [buncoverTile board revealedBoard'' (a,b) (x,(y-1))]
				revealedBoard'''' <- [buncoverTile board revealedBoard''' (a,b) ((x-1),(y-1))]
				buncoverTile board revealedBoard'''' (a,b) ((x-1),y)
			else do
				revealedBoard' <- [buncoverTile board revealedBoard (a,b) ((x-1),(y-1))]
				revealedBoard'' <- [buncoverTile board revealedBoard' (a,b) ((x-1),y)]
				revealedBoard''' <- [buncoverTile board revealedBoard'' (a,b) ((x-1),(y+1))]
				revealedBoard'''' <- [buncoverTile board revealedBoard''' (a,b) (x,(y+1))]
				revealedBoard''''' <- [buncoverTile board revealedBoard''' (a,b) ((x+1),(y+1))]
				revealedBoard''''' <- [buncoverTile board revealedBoard'''' (a,b) ((x+1),y)]
				revealedBoard'''''' <- [buncoverTile board revealedBoard''''' (a,b) ((x+1),(y-1))]
				buncoverTile board revealedBoard'''''' (a,b) (x,(y-1))
		else revealedBoard
--

-- Flag/unflag a hidden tile
flagRow :: [Char] -> Int -> [Char]
flagRow [] _ = []
flagRow (rowx:rowxs) 1 = flagged:rowxs
flagRow (rowx:rowxs) x = rowx:(flagRow rowxs (x-1))

flag :: [[Char]] -> (Int,Int) -> [[Char]]
flag [] (_,_) = []
flag (boardx:boardxs) (1,y) = (flagRow boardx y):boardxs
flag (boardx:boardxs) (x,y) = boardx:(flag boardxs ((x-1),y))

unflagRow :: [Char] -> Int -> [Char]
unflagRow [] _ = []
unflagRow (rowx:rowxs) 1 = hidden:rowxs
unflagRow (rowx:rowxs) x = rowx:(unflagRow rowxs (x-1))

unflag :: [[Char]] -> (Int,Int) -> [[Char]]
unflag [] (_,_) = []
unflag (boardx:boardxs) (1,y) = (unflagRow boardx y):boardxs
unflag (boardx:boardxs) (x,y) = boardx:(unflag boardxs ((x-1),y))
--

-- Endgame conditions
isLoseRow :: [Char] -> [Char] -> Bool
isLoseRow [] [] = False
isLoseRow (b:bs) (h:hs) = 
	if h == revealed && b == mine
		then True
		else isLoseRow bs hs

isLose :: [[Char]] -> [[Char]] -> Bool
isLose [] [] = False
isLose (b:bs) (h:hs) = isLoseRow b h || isLose bs hs

isWinRow :: [Char] -> [Char] -> Bool
isWinRow [] [] = True
isWinRow (b:bs) (h:hs) = 
	if (h == revealed && b /= mine) || (h /= revealed && b == mine)
		then isWinRow bs hs
		else False

isWin :: [[Char]] -> [[Char]] -> Bool
isWin [] [] = True
isWin (b:bs) (h:hs) = isWinRow b h && isWin bs hs
--

main :: IO()
main = do
	-- Game setup
	g <- newStdGen -- so
	let hiddenBoard = initBoard (10,10)
	let minesBoard = convertToField 
		[[(listArray ((0,0),(9,9)) ( myRands g ) :: Array (Int,Int) Int) -- so
		! (x, y) | x <- [0..9]] | y <- [0..9]] -- so
	let numbersBoard = computeNumbersFirst minesBoard (10,10)

	-- User prompt
	putStrLn ""
	putStrLn "Current Board"
	putStrLn $ unlines $ hiddenBoard
	putStrLn ""
	putStrLn "Enter coordinates to uncover in format: action x y"
	putStrLn "Actions: flag unflag uncover"
	putStrLn "Indexed from 1"
	putStrLn ""

	-- User input
	coords <- getLine
	let coords' = splitOn " " coords
	let action = (coords' !! 0)
	let x = read (coords' !! 1) :: Int
	let y = read (coords' !! 2) :: Int

	-- Make a move
	let revealedBoard =	case action of
		"uncover"	->	do
			buncoverTile numbersBoard hiddenBoard (10,10) (x,y)
		"flag"		->	do
			flag hiddenBoard (x,y)
		otherwise	->	do
			unflag hiddenBoard (x,y)
	let currentBoard = showCurrentBoard numbersBoard revealedBoard

	-- Show game state
	putStrLn "Current Board"
	putStrLn $ unlines $ currentBoard
	if (isLose numbersBoard revealedBoard) then print "LOSE"
	else print "NO LOSE"
	if (isWin numbersBoard revealedBoard) then print "WIN"
	else print "NO WIN"
	putStrLn ""