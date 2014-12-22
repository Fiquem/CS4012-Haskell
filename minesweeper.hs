
import System.Random
import Data.Array.IArray
import Data.Char
import Data.List.Split
import Data.Function (on)
import Data.List (sortBy, groupBy) 
import Test.QuickCheck

mine = 'o'
noMine = '_'

hidden = '#'
revealed = ' '
flagged = 'F'

data Difficulty = Difficulty { rows :: Int
							 , cols :: Int
							 , mines :: Int
							 } deriving (Show)

beginner = Difficulty 9 9 10
intermediate = Difficulty 16 16 40
expert = Difficulty 16 30 99

type Tile = Char
type Row = [Tile]
type RowAbove = Row
type RowBelow = Row
type InternalRow = Row
type InternalBoard = [Row]
type PlayerBoard = [Row]

type Coordinates = (Int,Int)

type Rows = Int
type Columns = Int
type BoardSize = (Rows,Columns)

type PositionInRow = Int
type RowSize = Int

type GameResult = IO [Char]

-- Build initial hidden board
initRow :: PositionInRow -> Row
initRow 0 = []
initRow x = hidden:(initRow (x-1))

initBoard :: BoardSize -> PlayerBoard
initBoard (0,_) = []
initBoard (x,y) = (initRow y):(initBoard ((x-1),y))
--

-- Current game state
showCurrentBoardRows :: Row -> Row -> Row
showCurrentBoardRows [] [] = []
showCurrentBoardRows (b:bs) (h:hs) = 
	case h of
		'#' -> h:(showCurrentBoardRows bs hs)
		'F' -> h:(showCurrentBoardRows bs hs)
		' ' -> b:(showCurrentBoardRows bs hs)

showCurrentBoard :: InternalBoard -> PlayerBoard -> PlayerBoard
showCurrentBoard [] [] = []
showCurrentBoard (b:bs) (h:hs) = (showCurrentBoardRows b h):(showCurrentBoard bs hs)
--

-- Game board with number of adjacent mines
makeStr :: Char -> Int -> String
makeStr _ 0 = []
makeStr x 1 = [x]
makeStr x n = x:(makeStr x (n-1))

sumAdj :: Tile -> Tile -> Tile -> Tile -> Tile -> Tile -> Tile -> Tile -> Tile
sumAdj a b c d e f g h
	| sum == 8*zero				= noMine
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
	zero = ord noMine
	one = ord mine

--	a b c
--	d ? e
--	f g h

computeNumbersRows :: RowAbove -> Row -> RowBelow -> RowSize -> Row
computeNumbersRows _ _ _ 0 = []
computeNumbersRows _ (_:'o':_) _ 1 = ['o']
computeNumbersRows (a0:a1:as) (b0:b1:bs) (c0:c1:cs) 1 = [sumAdj a0 a1 '_' b0 '_' c0 c1 '_']
computeNumbersRows (a0:a1:as) (_:'o':bs) (c0:c1:cs) y = 
	'o':(computeNumbersRows (a1:as) ('o':bs) (c1:cs) (y-1))
computeNumbersRows (a0:a1:a2:as) (b0:b1:b2:bs) (c0:c1:c2:cs) y = 
	(sumAdj a0 a1 a2 b0 b2 c0 c1 c2):(computeNumbersRows (a1:a2:as) (b1:b2:bs) (c1:c2:cs) (y-1))
computeNumbersRows a b c y = " "++[intToDigit y]++" "++a++" "++b++" "++c

computeNumbersRowsFirst :: RowAbove -> Row -> RowBelow -> RowSize -> Row
computeNumbersRowsFirst _ _ _ 0 = []
computeNumbersRowsFirst _ ('o':_) _ 1 = ['o']
computeNumbersRowsFirst (a:as) (b:bs) (c:cs) 1 = [sumAdj '_' a '_' '_' '_' '_' c '_']
computeNumbersRowsFirst (a0:as) ('o':bs) (c0:cs) y = 
	'o':(computeNumbersRows (a0:as) ('o':bs) (c0:cs) (y-1))
computeNumbersRowsFirst (a0:a1:as) (b0:b1:bs) (c0:c1:cs) y = 
	(sumAdj '_' a0 a1 '_' b1 '_' c0 c1):(computeNumbersRows (a0:a1:as) (b0:b1:bs) (c0:c1:cs) (y-1))
computeNumbersRowsFirst a b c y = " "++[intToDigit y]++" "++a++" "++b++" "++c

computeNumbers :: InternalBoard -> BoardSize -> InternalBoard
computeNumbers _ (0,_) = []
computeNumbers (b0:b1:bs) (1,y) = [computeNumbersRowsFirst b0 b1 (makeStr '_' y) y]
computeNumbers (b0:b1:b2:bs) (x,y) = (computeNumbersRowsFirst b0 b1 b2 y):(computeNumbers (b1:b2:bs) ((x-1),y))

computeNumbersFirst :: InternalBoard -> BoardSize -> InternalBoard
computeNumbersFirst board (0,_) = []
computeNumbersFirst (b:bs) (1,y) = [computeNumbersRowsFirst (makeStr '_' y) b (makeStr '_' y) y]
computeNumbersFirst (b0:b1:bs) (x,y) = 
	(computeNumbersRowsFirst (makeStr '_' y) b0 b1 y):(computeNumbers (b0:b1:bs) ((x-1),y))
--

-- Uncover tile and all tiles adjacent if it's a non-numbered tile
findTileRow :: Row -> PositionInRow -> Tile
findTileRow (rowx:rowxs) 1 = rowx
findTileRow (rowx:rowxs) x = findTileRow rowxs (x-1)

findTile :: InternalBoard -> Coordinates -> Tile
findTile (boardx:boardxs) (1,y) = findTileRow boardx y
findTile (boardx:boardxs) (x,y) = findTile boardxs ((x-1),y)

createListOfAdjacentTiles :: Coordinates -> [Coordinates]
createListOfAdjacentTiles (x,y) = [((x-1),(y-1)),((x-1),y),((x-1),(y+1)),(x,(y+1)),((x+1),(y+1)),((x+1),y),((x+1),(y-1)),(x,(y-1))]

uncoverAdjacentTiles :: InternalBoard -> PlayerBoard -> Coordinates -> [Coordinates] -> PlayerBoard
uncoverAdjacentTiles board revealedBoard boardSize [] = revealedBoard
uncoverAdjacentTiles board revealedBoard boardSize (tile:tiles) = 
	uncoverAdjacentTiles board revealedBoard' boardSize tiles
	where revealedBoard' = buncoverTile board revealedBoard boardSize tile

uncoverTileRow :: Row -> PositionInRow -> Row
uncoverTileRow [] _ = []
uncoverTileRow (rowx:rowxs) 1 = 
	case rowx of
		'F' -> rowx:rowxs
		_	-> revealed:rowxs
uncoverTileRow (rowx:rowxs) x = rowx:(uncoverTileRow rowxs (x-1))

uncoverTile :: PlayerBoard -> Coordinates -> PlayerBoard
uncoverTile [] (_,_) = []
uncoverTile (boardx:boardxs) (1,y) = (uncoverTileRow boardx y):boardxs
uncoverTile (boardx:boardxs) (x,y) = boardx:(uncoverTile boardxs ((x-1),y))

buncoverTile :: InternalBoard -> PlayerBoard -> BoardSize -> Coordinates -> PlayerBoard
buncoverTile board revealedBoard (a,b) (x,y) = 
	if x /= 0 && x /= (a+1) && y /= 0 && y /= (b+1) && findTile revealedBoard (x,y) == '#'
		then if findTile board (x,y) == '_'
			then uncoverAdjacentTiles board (uncoverTile revealedBoard (x,y)) (a,b) (createListOfAdjacentTiles (x,y))
			else (uncoverTile revealedBoard (x,y))
		else revealedBoard
--

-- Flag/unflag a hidden tile
flagRow :: Row -> PositionInRow -> Row
flagRow [] _ = []
flagRow (rowx:rowxs) 1 = flagged:rowxs
flagRow (rowx:rowxs) x = rowx:(flagRow rowxs (x-1))

flag :: PlayerBoard -> Coordinates -> PlayerBoard
flag [] (_,_) = []
flag (boardx:boardxs) (1,y) = 
	if findTile (boardx:boardxs) (1,y) == '#'
		then (flagRow boardx y):boardxs
		else (boardx:boardxs)
flag (boardx:boardxs) (x,y) = 
	if findTile (boardx:boardxs) (x,y) == '#'
		then boardx:(flag boardxs ((x-1),y))
		else (boardx:boardxs)	

unflagRow :: Row -> PositionInRow -> Row
unflagRow [] _ = []
unflagRow (rowx:rowxs) 1 = hidden:rowxs
unflagRow (rowx:rowxs) x = rowx:(unflagRow rowxs (x-1))

unflag :: PlayerBoard -> Coordinates -> PlayerBoard
unflag [] (_,_) = []
unflag (boardx:boardxs) (1,y) = 
	if findTile (boardx:boardxs) (1,y) == 'F'
		then (unflagRow boardx y):boardxs
		else (boardx:boardxs)
unflag (boardx:boardxs) (x,y) = 
	if findTile (boardx:boardxs) (x,y) == 'F'
		then boardx:(unflag boardxs ((x-1),y))
		else (boardx:boardxs)
--

-- Endgame conditions
isLoseRow :: Row -> Row -> Bool
isLoseRow [] [] = False
isLoseRow (b:bs) (h:hs) = 
	if h == revealed && b == mine
		then True
		else isLoseRow bs hs

isLose :: InternalBoard -> PlayerBoard -> Bool
isLose [] [] = False
isLose (b:bs) (h:hs) = isLoseRow b h || isLose bs hs

isWinRow :: Row -> Row -> Bool
isWinRow [] [] = True
isWinRow (b:bs) (h:hs) = 
	if (h == revealed && b /= mine) || (h /= revealed && b == mine)
		then isWinRow bs hs
		else False

isWin :: InternalBoard -> PlayerBoard -> Bool
isWin [] [] = True
isWin (b:bs) (h:hs) = isWinRow b h && isWin bs hs
--

-- Play the game!
playTurn :: String -> InternalBoard -> PlayerBoard -> Difficulty -> Coordinates -> PlayerBoard
playTurn action board revealedBoard difficulty (x,y) =
	case action of
		"uncover"	->	do
			buncoverTile board revealedBoard ((rows difficulty),(cols difficulty)) (x,y)
		"flag"		->	do
			flag revealedBoard (x,y)
		otherwise	->	do
			unflag revealedBoard (x,y)

getUserInputAndMakeSureIt'sNotShittyInput :: Difficulty -> IO [[Char]]
getUserInputAndMakeSureIt'sNotShittyInput difficulty = do
	coords <- getLine
	let coords' = splitOn " " coords
	if length coords' == 3 && ((coords' !! 0) :: String) `elem` ["flag","unflag","uncover"] && 
		(read (coords' !! 1) :: Int) `elem` [1..(cols difficulty)] && (read (coords' !! 2) :: Int) `elem` [1..(rows difficulty)]
		then return coords'
		else if length coords' == 1 && ((coords' !! 0) :: String) == "playMove"
			then return (coords'++[" 0 0"])
			else do
				putStrLn "No. Input again."
				getUserInputAndMakeSureIt'sNotShittyInput difficulty

playGame :: InternalBoard -> PlayerBoard -> Difficulty -> GameResult
playGame board revealedBoard difficulty = do
	-- User prompt
	putStrLn "Enter coordinates to uncover in format: action x y"
	putStrLn "Actions: flag unflag uncover"
	putStrLn "Indexed from 1"
	putStrLn ""

	-- User input
	coords <- getUserInputAndMakeSureIt'sNotShittyInput difficulty
	let action = (coords !! 0)
	let x = read (coords !! 2) :: Int
	let y = read (coords !! 1) :: Int
	putStrLn ""

	-- Make a move
	let revealedBoard' = 
		if action == "playMove"
			then playMove board revealedBoard difficulty
			else playTurn action board revealedBoard difficulty (x,y)

	--let (revealedBoard',(somethingx,somethingy)) = 
	--	if action == "playMove"
	--		then playMove board revealedBoard difficulty
	--		else (playTurn action board revealedBoard difficulty (x,y),(0,0))

	--print somethingx
	--print somethingy
	--print "LOOK AT THIS"

	-- Show game state
	let currentBoard = showCurrentBoard board revealedBoard'
	putStrLn ""
	putStrLn "Current Board"
	putStrLn $ unlines $ currentBoard

	-- Detect endgame or repeat
	if (isLose board revealedBoard') then return "LOSE"
	else if (isWin board revealedBoard') then return "WIN"
	else playGame board revealedBoard' difficulty
--

-- Building board, placing mines
randPerm :: StdGen -> [Coordinates] -> [Coordinates]
randPerm _ []   = []
randPerm gen xs = let (n,newGen) = randomR (0,length xs -1) gen
                      front = xs !! n
                  in  front : randPerm newGen (take n xs ++ drop (n+1) xs)

sortGT :: Coordinates -> Coordinates -> Ordering
sortGT (a1,b1) (a2,b2) = 
  case compare a1 a2 of
    EQ -> compare b2 b1
    LT -> GT
    GT -> LT

getRandomMinePositions :: StdGen -> [Coordinates] -> Int -> [[Coordinates]]
getRandomMinePositions gen possiblePositions numMines = groupByRow (sortBy sortGT (take numMines (randPerm gen possiblePositions)))

groupByRow :: [Coordinates] -> [[Coordinates]]
groupByRow xs = groupBy (\x y -> fst x == fst y) xs

placeMinesRow :: [Coordinates] -> PositionInRow -> Row
placeMinesRow _ 0 = []
placeMinesRow [] y = noMine:(placeMinesRow [] (y-1))
placeMinesRow ((mx,my):ms) y = 
	if my == y
		then mine:(placeMinesRow ms (y-1))
		else noMine:(placeMinesRow ((mx,my):ms) (y-1))

placeMines :: [[Coordinates]] -> Coordinates -> InternalBoard
placeMines _ (0,_) = []
placeMines [] (x,y) = (placeMinesRow [] y):(placeMines [] ((x-1),y))
placeMines (((rowMx,rowMy):rowMs):ms) (x,y) = 
	if rowMx == x
		then (placeMinesRow ((rowMx,rowMy):rowMs) y):(placeMines ms ((x-1),y))
		else (placeMinesRow [] y):(placeMines (((rowMx,rowMy):rowMs):ms) ((x-1),y))

generateRandomBoard :: StdGen -> Difficulty -> InternalBoard
generateRandomBoard gen difficulty = placeMines ms ((rows difficulty),(cols difficulty))
	where ms = getRandomMinePositions gen possiblePositions (mines difficulty)
		where possiblePositions = allPossibleBoardPositions (rows difficulty) (cols difficulty)

allPossibleBoardPositions :: Rows -> Columns -> [Coordinates]
allPossibleBoardPositions 0 _ = []
allPossibleBoardPositions rowNum rowLen = allPossibleBoardPositions (rowNum-1) rowLen ++ zip (replicate rowLen rowNum) (cycle [1..rowLen])
--

-- Solver oh dear

numAdjUnrevealedTiles :: [Char] -> Int
numAdjUnrevealedTiles [] = 0
numAdjUnrevealedTiles (x:xs) = 
	if x == ' '
		then 0 + (numAdjUnrevealedTiles xs)
		else 1 + (numAdjUnrevealedTiles xs)

findCoordOfAdjacentCoveredTile :: Tile -> Tile -> Tile -> Tile -> Tile -> Tile -> Tile -> Tile -> Int -> Coordinates
findCoordOfAdjacentCoveredTile a b c d e f g h y
	| a == hidden	= (1,y+1)
	| b == hidden	= (1,y)
	| c == hidden	= (1,y-1)
	| d == hidden	= (0,y+1)
	| e == hidden	= (0,y-1)
	| f == hidden	= (-1,y+1)
	| g == hidden	= (-1,y)
	| h == hidden	= (-1,y-1)
	| otherwise		= (0,0) -- Tile is already flagged

isMineRows :: InternalRow -> RowAbove -> Row -> RowBelow -> RowSize -> (Bool,Coordinates)
isMineRows _ _ _ _ 0 = (False,(4,1))
isMineRows _ _ (_:'#':_) _ 1 = (False,(4,2))
isMineRows (x:xs) (a0:a1:as) (_:'#':bs) (c0:c1:cs) y = isMineRows xs (a1:as) ('#':bs) (c1:cs) (y-1)
isMineRows (x:xs) (a0:a1:as) (b0:b1:bs) (c0:c1:cs) 1 = 
	if x `notElem` ['_','o'] && digitToInt x == numAdjUnrevealedTiles [a0,a1,' ',b0,' ',c0,c1,' '] && (findCoordOfAdjacentCoveredTile a0 a1 ' ' b0 ' ' c0 c1 ' ' 1) /= (0,0)
		then (True,findCoordOfAdjacentCoveredTile a0 a1 ' ' b0 ' ' c0 c1 ' ' 1)
		else (False,(4,3))
isMineRows (x:xs) (a0:a1:a2:as) (b0:b1:b2:bs) (c0:c1:c2:cs) y = 
	if x `notElem` ['_','o'] && digitToInt x == numAdjUnrevealedTiles [a0,a1,a2,b0,b2,c0,c1,c2] && (findCoordOfAdjacentCoveredTile a0 a1 a2 b0 b2 c0 c1 c2 y) /= (0,0)
		then (True,findCoordOfAdjacentCoveredTile a0 a1 a2 b0 b2 c0 c1 c2 y)
		else isMineRows xs (a1:a2:as) (b1:b2:bs) (c1:c2:cs) (y-1)

isMineRowsFirst :: InternalRow -> RowAbove -> Row -> RowBelow -> RowSize -> (Bool,Coordinates)
isMineRowsFirst _ _ _ _ 0 = (False,(3,1))
isMineRowsFirst _ _ ('#':_) _ 1 = (False,(3,2))
isMineRowsFirst (x:xs) (a0:as) ('#':bs) (c0:cs) y = isMineRows xs (a0:as) ('#':bs) (c0:cs) (y-1)
isMineRowsFirst (x:xs) (a:as) (' ':bs) (c:cs) 1 = 
	if x `notElem` ['_','o'] && digitToInt x == numAdjUnrevealedTiles [' ',a,' ',' ',' ',' ',c,' '] && (findCoordOfAdjacentCoveredTile ' ' a ' ' ' ' ' ' ' ' c ' ' 1) /= (0,0)
		then (True,findCoordOfAdjacentCoveredTile ' ' a ' ' ' ' ' ' ' ' c ' ' 1)
		else (False,(3,3))
isMineRowsFirst (x:xs) (a0:a1:as) (b0:b1:bs) (c0:c1:cs) y = 
	if x `notElem` ['_','o'] && digitToInt x == numAdjUnrevealedTiles [' ',a0,a1,' ',b1,' ',c0,c1] && (findCoordOfAdjacentCoveredTile  ' ' a0 a1 ' ' b1 ' ' c0 c1 y) /= (0,0)
		then (True,findCoordOfAdjacentCoveredTile ' ' a0 a1 ' ' b1 ' ' c0 c1 y)
		else isMineRows xs (a0:a1:as) (b0:b1:bs) (c0:c1:cs) (y-1)

isMine :: InternalBoard -> PlayerBoard -> BoardSize -> (Bool,Coordinates)
isMine _ _ (0,_) = (False,(2,2))
isMine (b0:b1:bs) (h0:h1:hs) (1,y) = isMineRowsFirst b1 h0 h1 (makeStr ' ' y) y
isMine (b0:b1:b2:bs) (h0:h1:h2:hs) (x,y) = 
	if (isMineRowsFirst b1 h0 h1 h2 y) == (True,coords)
		then (True,addCoords coords (x,0))
		else isMine (b1:b2:bs) (h1:h2:hs) ((x-1),y)
	where (_,coords) = (isMineRowsFirst b1 h0 h1 h2 y)

isMineFirst :: InternalBoard -> PlayerBoard -> BoardSize -> (Bool,Coordinates)
isMineFirst _ _ (0,_) = (False,(1,1))
isMineFirst (b:bs) (h:hs) (1,y) = isMineRowsFirst b (makeStr ' ' y) h (makeStr ' ' y) y
isMineFirst (b0:b1:bs) (h0:h1:hs) (x,y) = 
	if (isMineRowsFirst b0 (makeStr ' ' y) h0 h1 y) == (True,coords)
		then (True,addCoords coords (x,0))
		else isMine (b0:b1:bs) (h0:h1:bs) ((x-1),y)
	where (_,coords) = (isMineRowsFirst b0 (makeStr ' ' y) h0 h1 y)

--showCoords :: Coordinates -> String
--showCoords (x,y) = [(intToDigit x)]++" "++[(intToDigit y)]

addCoords :: Coordinates -> Coordinates -> Coordinates
addCoords (x0,y0) (x1,y1) = (x0+x1,y0+y1)

subCoords :: Coordinates -> Coordinates -> Coordinates
subCoords (x0,y0) (x1,y1) = (x0-x1,y0-y1)

findFirstHiddenTileRows :: Row -> PositionInRow -> RowSize -> Int
findFirstHiddenTileRows b x y = 
	if x > y 
		then 0
		else if findTileRow b x == '#'
			then x
			else findFirstHiddenTileRows b (x+1) y

findFirstHiddenTile :: PlayerBoard -> BoardSize -> Coordinates
findFirstHiddenTile (b:bs) (x,y) = 
	if (findFirstHiddenTileRows b 1 y) /= 0
		then (x,(y+1)-findFirstHiddenTileRows b 1 y)
		else (findFirstHiddenTile bs ((x-1,y)))

--playMove :: InternalBoard -> PlayerBoard -> Difficulty -> (PlayerBoard,Coordinates)
playMove :: InternalBoard -> PlayerBoard -> Difficulty -> PlayerBoard
playMove board revealedBoard difficulty = 
	-- Initial move
	--playTurn "uncover" board revealedBoard difficulty (subCoords (rows difficulty,cols difficulty) (subCoords (findFirstHiddenTile revealedBoard (rows difficulty,cols difficulty)) (1,1)))
	-- Flag known mine
	if (isMineFirst board revealedBoard (rows difficulty,cols difficulty)) == (True,coords)
		--then (playTurn "flag" board revealedBoard difficulty (subCoords (rows difficulty,cols difficulty) (subCoords coords (1,1))),coords)
		--else (playTurn "uncover" board revealedBoard difficulty (subCoords (rows difficulty,cols difficulty) (subCoords (findFirstHiddenTile revealedBoard (rows difficulty,cols difficulty)) (1,1))),coords)
		then playTurn "flag" board revealedBoard difficulty (subCoords (rows difficulty,cols difficulty) (subCoords coords (1,1)))
		else playTurn "uncover" board revealedBoard difficulty (subCoords (rows difficulty,cols difficulty) (subCoords (findFirstHiddenTile revealedBoard (rows difficulty,cols difficulty)) (1,1)))
	where (_,coords) = (isMineFirst board revealedBoard (rows difficulty,cols difficulty))
--

main :: IO()
main = do
	-- Game setup
	gen <- getStdGen -- random number generator seed
	let difficulty = beginner
	let initialBoard = initBoard (rows difficulty,cols difficulty)
	let minesBoard = generateRandomBoard gen difficulty
	let internalBoard = computeNumbersFirst minesBoard (rows difficulty,cols difficulty)

	-- Play game!
	putStrLn ""
	putStrLn "Current Board"
	putStrLn $ unlines $ initialBoard
	result <- playGame internalBoard initialBoard difficulty
	putStrLn result