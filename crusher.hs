-- Crusher outline
import Data.Char
import Data.List
import Data.Ord

type Game = (Board, Int) -- Probably change to a data
type Board = [(Position, Piece)]
data Position = Pos Int Int
				deriving (Ord, Eq, Show)
type Piece = Char

--Added stuff here

getBestMove_c5n7 :: String -> Char -> Int -> Int -> [String] -> [String]
getBestMove_c5n7 board side depth size history
	= (firstMove : [board]) ++ history
	where 
		firstMove = (listOfMoves)!!1
		listOfMoves = reverse (playCrusher_c5n7 board depth side size history)

---stuff ends here

crusher_c5n7 :: String -> Char -> Int -> Int -> [String] -> [String]
crusher_c5n7 board side depth size history = 
	(toString_c5n7 
		(fst (crusher'_c5n7
				(head (makeBoards_c5n7 size [board]))  
				(toUpper side) 
				0 
				depth 
				(makeBoards_c5n7 size history)))) : (board : history)

-- Consumes a board and converts it to a list of String.
toString_c5n7 :: Board -> String
toString_c5n7 board = getRows_c5n7 board 1

-- Helper function for toListOfString_c5n7. Consumes a Board, board, and an Integer, 
-- row, to convert into a string and produces a list of String by combining
-- each row together into a list.
getRows_c5n7 :: Board -> Int -> String
getRows_c5n7 board row
	| row == (getMax_c5n7 board) + 1	= []
	| otherwise					
		= (getString_c5n7 board row) ++ getRows_c5n7 board (row + 1)
		
-- Consumes a Board and produces the maximum column value on the board. Allows
-- getRows to terminate appropriately, and Boards of different sizes to be 
-- processed.
getMax_c5n7 :: Board -> Int
getMax_c5n7 board = maximum [n | (Pos m n, char) <- board]

-- Consumes a Board, board, and an Integer, row, and produces a String 
-- corresponding to that row in board. Collects all the character values 
-- associated with the row, row and creates a String from them.
getString_c5n7 :: Board -> Int -> String
getString_c5n7 board row = [char | (Pos m n, char) <- (sort board), m == row]
									
crusher'_c5n7 :: Board -> Char -> Int -> Int -> [Board] -> Game
crusher'_c5n7 board side currDepth depth history
	| currDepth == depth	= game
	| gameOver_c5n7 board generatedBoards
							= (board, (gameEndScore_c5n7 currDepth))
	| currDepth == 0		= miniMax_c5n7 currDepth evaluatedChildren
	| otherwise 			= (board, (snd nextGame))
	where 
		game = makeHeuristic_c5n7 side board
		nextGame = miniMax_c5n7 currDepth evaluatedChildren
		evaluatedChildren = runCrusherOnEach 	generatedBoards 
												side 
												(currDepth + 1) 
												depth 
												(board:history)
		generatedBoards = generateBoards_c5n7 board side currDepth history

runCrusherOnEach [] _ _ _ _ = []
runCrusherOnEach (board:boards) side currDepth depth history =
	(crusher'_c5n7 board side currDepth depth history) :
				(runCrusherOnEach boards side currDepth depth history)

gameOver_c5n7 :: Board -> [Board] -> Bool		   
gameOver_c5n7 board [] = True 
gameOver_c5n7 board _ = 
	(notEnoughPieces_c5n7 board 'W') || (notEnoughPieces_c5n7 board 'B') 

notEnoughPieces_c5n7 :: Board -> Char -> Bool
notEnoughPieces_c5n7 board side = 
	(length (getSide_c5n7 side board)) < (getSize_c5n7 board)

getSize_c5n7 :: Board -> Int
getSize_c5n7 board = length [col | (Pos row col, _) <- board, row == 1] 
		
generateBoards_c5n7 :: Board -> Char -> Int -> [Board] -> [Board]
generateBoards_c5n7 board side depth history 
	= (generateBoards'_c5n7 board history (getSide_c5n7 currMove board))
	where currMove = if ((mod depth 2) == 1) then (otherSide_c5n7 side) else side
	
otherSide_c5n7 :: Char -> Char
otherSide_c5n7 side = if (side == 'w' || side == 'W') then 'B' else 'W'

generateBoards'_c5n7 :: Board -> [Board] -> [(Position, Piece)] -> [Board]	
generateBoards'_c5n7 _ _ [] = []
generateBoards'_c5n7 board history (piece:pieces) = 
	(generateBoardsFromPiece_c5n7 board history piece) ++ 
	(generateBoards'_c5n7 board history pieces)

getSide_c5n7 :: Char -> Board -> [(Position, Piece)]
getSide_c5n7 side board = [(pos, char) | (pos, char) <- board, char == side]

generateBoardsFromPiece_c5n7 :: Board -> [Board] -> (Position, Piece) -> [Board]
generateBoardsFromPiece_c5n7 board history piece =
	filterHistory_c5n7 ((generateUps_c5n7 board piece) ++ 
					(generateDowns_c5n7 board piece) ++
					(generateHorizontal_c5n7 board piece)) history
	
filterHistory_c5n7 :: [Board] -> [Board] -> [Board]
filterHistory_c5n7 loboards history = 
	[board | board <- loboards, not (elem board history)]

generateUps_c5n7 :: Board -> (Position, Piece) -> [Board]	
generateUps_c5n7 board piece = (slideUpLeft_c5n7 board piece) ++ 
							(slideUpRight_c5n7 board piece) ++
							(jumpUpLeft_c5n7 board piece) ++
							(jumpUpRight_c5n7 board piece)

generateDowns_c5n7 :: Board -> (Position, Piece) -> [Board]
generateDowns_c5n7 board piece = (slideDownLeft_c5n7 board piece) ++
							(slideDownRight_c5n7 board piece) ++
							(jumpDownLeft_c5n7 board piece) ++
							(jumpDownRight_c5n7 board piece)
							
generateHorizontal_c5n7 :: Board -> (Position, Piece) -> [Board]							
generateHorizontal_c5n7 board piece = (slideLeft_c5n7 board piece) ++
									(slideRight_c5n7 board piece) ++
									(jumpLeft_c5n7 board piece) ++
									(jumpRight_c5n7 board piece)

doSlide_c5n7 :: Board -> (Position, Piece) -> Position -> [Board]
doSlide_c5n7 board (pos, colour) newPos	
	| isEmpty_c5n7 board newPos = 
		-- isEmpty checks if a location is '-' so also checks if it is on board
		[(replaceChars_c5n7 board colour pos newPos)]
	| otherwise	= []

slideUpLeft_c5n7 :: Board -> (Position, Piece) -> [Board]	
slideUpLeft_c5n7 board (pos, colour) = doSlide_c5n7 board (pos, colour) newPos
		where newPos = (findUpLeft_c5n7 board pos)

slideUpRight_c5n7 :: Board -> (Position, Piece) -> [Board] 	
slideUpRight_c5n7 board (pos, colour) = doSlide_c5n7 board (pos, colour) newPos
		where newPos = (findUpRight_c5n7 board pos)

doJump_c5n7 :: Board -> (Position, Piece) -> Position -> Position -> [Board] 
doJump_c5n7 board (pos, colour) jumpPos newPos 
	| isGoodJump_c5n7 board colour jumpPos newPos
		= [(replaceChars_c5n7 board colour pos newPos)]
	| otherwise = []

jumpUpLeft_c5n7 :: Board -> (Position, Piece) -> [Board] 		
jumpUpLeft_c5n7 board (pos, colour) = doJump_c5n7 board (pos, colour) jumpPos newPos 
		where
			jumpPos = findUpLeft_c5n7 board pos 
			newPos = findUpLeft_c5n7 board jumpPos

jumpUpRight_c5n7 :: Board -> (Position, Piece) -> [Board] 
jumpUpRight_c5n7 board (pos, colour) = doJump_c5n7 board (pos, colour) jumpPos newPos 
		where 
			jumpPos = findUpRight_c5n7 board pos 
			newPos = findUpRight_c5n7 board jumpPos

slideDownLeft_c5n7 :: Board -> (Position, Piece) -> [Board] 
slideDownLeft_c5n7 board (pos, colour) = doSlide_c5n7 board (pos, colour) newPos
		where newPos = (findDownLeft_c5n7 board pos)
	
slideDownRight_c5n7 :: Board -> (Position, Piece) -> [Board] 
slideDownRight_c5n7 board (pos, colour) = doSlide_c5n7 board (pos, colour) newPos
		where newPos = (findDownRight_c5n7 board pos)

jumpDownLeft_c5n7 :: Board -> (Position, Piece) -> [Board] 
jumpDownLeft_c5n7 board (pos, colour) = doJump_c5n7 board (pos, colour) jumpPos newPos 
		where 	
			jumpPos = findDownLeft_c5n7 board pos
			newPos = findDownLeft_c5n7 board jumpPos

jumpDownRight_c5n7 :: Board -> (Position, Piece) -> [Board] 
jumpDownRight_c5n7 board (pos, colour) = doJump_c5n7 board (pos, colour) jumpPos newPos 
		where 	
			jumpPos = findDownRight_c5n7 board pos 
			newPos = findDownRight_c5n7 board jumpPos

slideLeft_c5n7 :: Board -> (Position, Piece) -> [Board] 			
slideLeft_c5n7 board (Pos row col, colour) = 
	doSlide_c5n7 board (Pos row col, colour) newPos
		where newPos = (Pos row (col - 1))
	
slideRight_c5n7 :: Board -> (Position, Piece) -> [Board] 				
slideRight_c5n7 board (Pos row col, colour) =
	doSlide_c5n7 board (Pos row col, colour) newPos
		where newPos = (Pos row (col + 1))

jumpLeft_c5n7 :: Board -> (Position, Piece) -> [Board] 
jumpLeft_c5n7 board (Pos row col, colour) = 
	doJump_c5n7 board (Pos row col, colour) jumpPos newPos 
		where 	
			jumpPos = Pos row (col - 1) 
			newPos = Pos row (col - 2)

jumpRight_c5n7 :: Board -> (Position, Piece) -> [Board] 
jumpRight_c5n7 board (Pos row col, colour) = 
	doJump_c5n7 board (Pos row col, colour) jumpPos newPos 
		where 	
			jumpPos = Pos row (col + 1) 
			newPos = Pos row (col + 2)

isGoodJump_c5n7 :: Board -> Char -> Position -> Position -> Bool
isGoodJump_c5n7 board colour jumpPos newPos =
	(isSame_c5n7 board colour jumpPos) && (isDifferent_c5n7 board colour newPos)

findUpLeft_c5n7 :: Board -> Position -> Position
findUpLeft_c5n7 board (Pos row col) 
	| row > (midRow_c5n7 row board)	= Pos (row - 1) col
	| otherwise 					= Pos (row - 1) (col - 1)

findUpRight_c5n7 :: Board -> Position -> Position
findUpRight_c5n7 board (Pos row col) 
	| row > (midRow_c5n7 row board)	= Pos (row - 1) (col + 1)
	| otherwise 					= Pos (row - 1) col

findDownLeft_c5n7 :: Board -> Position -> Position	
findDownLeft_c5n7 board (Pos row col)
	| row >= (midRow_c5n7 row board) = Pos (row + 1) (col - 1)
	| otherwise 					 = Pos (row + 1) col

findDownRight_c5n7 :: Board -> Position -> Position	
findDownRight_c5n7 board (Pos row col)
	| row >= (midRow_c5n7 row board) = Pos (row + 1) col
	| otherwise 					 = Pos (row + 1) (col + 1)

midRow_c5n7 :: Int -> Board -> Int		
midRow_c5n7 row board = head [row | ((Pos row col), _) <- board, col == maxCol]
	where maxCol = maximum [col |((Pos _ col), _) <- board] 

makeBoards_c5n7 :: Int -> [String] -> [Board]
makeBoards_c5n7 size lob = [(makeBoard' board size) | board <- lob]
	where makeBoard' board size = 
		 (makeBoard_c5n7 (splitIntoRows_c5n7 size board) 1 size)

makeHeuristic_c5n7 :: Char -> Board -> Game
makeHeuristic_c5n7 side board = (board, addScores_c5n7 board side) 

addScores_c5n7 :: Board -> Char -> Int
addScores_c5n7 board side = (winPoints_c5n7 board side) + (piecePoints_c5n7 board side)

winPoints_c5n7 :: Board -> Char -> Int
winPoints_c5n7 board side 
	 | notEnoughPieces_c5n7 board side					= lossValue_c5n7
	 | notEnoughPieces_c5n7 board (otherSide_c5n7 side)	= winValue_c5n7
	 | otherwise 									= 0

gameEndScore_c5n7 :: Int -> Int
gameEndScore_c5n7 depth = if ((mod depth 2) == 1) 
							then winValue_c5n7 * 10 
							else lossValue_c5n7 * 10
winValue_c5n7 = 10
lossValue_c5n7 = -10

piecePoints_c5n7 :: Board -> Char -> Int
piecePoints_c5n7 board side = 
	(length (getSide_c5n7 side board)) - 
	(length (getSide_c5n7 (otherSide_c5n7 side) board))

makeBoard_c5n7 :: [String] -> Int -> Int -> Board
makeBoard_c5n7 (str:los) curr size = 
	if (curr == ((2 * size) - 1)) 
	then (makeRow_c5n7 str curr 1)
	else ((makeRow_c5n7 str curr 1) ++ (makeBoard_c5n7 los (curr + 1) size))

makeRow_c5n7 :: String -> Int -> Int -> Board
makeRow_c5n7 [] row col = []
makeRow_c5n7 (ch: loc) row col = 
	((Pos row col), (toUpper ch)) : (makeRow_c5n7 loc row (col + 1))

getHr_c5n7 :: Game -> Int
getHr_c5n7 game = snd game 

addHr_c5n7 :: Game -> Int -> Game
addHr_c5n7 game value = ((fst game), ((snd game) + value))

miniMax_c5n7 :: Int -> [Game] -> Game
-- miniMax_c5n7 depth [] = []  -- This shoudln't ever happen
miniMax_c5n7 depth logame =
	if ((mod depth 2) == 1) 
		then (minimumBy (comparing snd) logame)
		else (maximumBy (comparing snd) logame)

splitIntoRows_c5n7 :: Int -> String -> [String]
splitIntoRows_c5n7 n board 
	= splitHelper_c5n7 board 1 n n

splitHelper_c5n7 :: String -> Int -> Int -> Int -> [String]
splitHelper_c5n7 inputString currRow colsInRow size
	| currRow >= (2 * size - 1) 	= [(take colsInRow inputString)]
	| currRow < size		
		= (take colsInRow inputString) : 
				splitHelper_c5n7 	(drop colsInRow inputString) 
									(currRow + 1)
									(colsInRow + 1)
									size
	| otherwise					
		= (take colsInRow inputString) : 
				splitHelper_c5n7 	(drop colsInRow inputString) 
									(currRow + 1)
									(colsInRow - 1)
									size
				

withinBoard_c5n7 :: Board -> Position -> Bool
withinBoard_c5n7 board pos
	= (elem True [(fst b) == pos | b <- board])

isSame_c5n7 :: Board -> Char -> Position -> Bool
isSame_c5n7 board side pos = ((getElement_c5n7 board pos) == [side])

isDifferent_c5n7 :: Board -> Char -> Position -> Bool
isDifferent_c5n7 board side pos 
	| (toUpper side) == 'W' 	=  elem targetPiece [['B'], ['-']]
	| (toUpper side) == 'B'		=  elem targetPiece [['W'], ['-']]
	| otherwise					= False 
		where targetPiece = getElement_c5n7 board pos

getElement_c5n7 :: Board -> Position -> [Char]
getElement_c5n7 board targetPos = 
	[char | (pos, char) <- board, pos == targetPos]

isEmpty_c5n7 :: Board -> Position -> Bool
isEmpty_c5n7 board pos 
	= [char | (posToCheck, char) <- board, pos == posToCheck] == ['-']

replaceChars_c5n7 :: Board -> Char -> Position -> Position -> Board
replaceChars_c5n7 board side from_pos to_pos
	= [(replace' side (pos, char)) | (pos, char) <- board]
		where replace' side (pos, char)
				| (pos == from_pos) = (pos, '-')
				| (pos == to_pos) = (pos, side)
				| otherwise = (pos, char)


testMakeBoards0 = makeBoards_c5n7 2 ["-wb-wb-"]
testMakeBoards1 = makeBoards_c5n7 3 ["www-ww-------bb-bbb"]
testMakeBoards2 = makeBoards_c5n7 3 ["www-ww-------bb-bbb", "www-w-w------bb-bbb"]
testMakeBoards3 = makeBoards_c5n7 4 ["WW----WB---BB------------------------"]

testSplitStrings = splitIntoRows_c5n7 4 "WW----WB---BB------------------------"

testB1 = head (makeBoards_c5n7 3 ["w------------------"])
testB2 = head (makeBoards_c5n7 3 ["-b-----------------"])
testB3 = head (makeBoards_c5n7 3 ["----w--------------"])
testB4 = head (makeBoards_c5n7 3 ["--------------b----"])
testB5 = head (makeBoards_c5n7 3 ["--------------bb---"])
testB6 = head (makeBoards_c5n7 3 ["---------b----b----"])
testB7 = head (makeBoards_c5n7 3 ["WW-------B---BB-BB-"])
testB8 = head (makeBoards_c5n7 3 ["WWW------B---BB-BB-"])
testB9 = head (makeBoards_c5n7 3 ["WWWWWWWWWBBBBBBBBBB"])
testB10 = head (makeBoards_c5n7 3 ["--W------WW-BWB----"])
testB11 = head (makeBoards_c5n7 4 ["WW----WB---BB------------------------"])
testB12 = head (makeBoards_c5n7 3 ["WW--BB--BB---B---W-"])

testGenerateBoards0 = generateBoards_c5n7 (head testMakeBoards1) 'W' 1 []
testGenerateBoards1 = generateBoards_c5n7 testB7 'W' 1 []
testGenerateBoards2 = generateBoards_c5n7 testB8 'W' 1 []
testGenerateBoards3 = generateBoards_c5n7 testB10 'W' 1 []
testGenerateBoards4 = generateBoards_c5n7 testB11 'W' 1 []
testGenerateBoards5 = generateBoards_c5n7 testB12 'W' 1 []

testCrushOnEach0 = runCrusherOnEach testGenerateBoards5 'B' 1 3 [testB12]
board1 = [(Pos 1 1,'W'),(Pos 1 2,'W'),(Pos 1 3,'-'),(Pos 2 1,'-'),(Pos 2 2,'B'),(Pos 2 3,'B'),(Pos 2 4,'-'),(Pos 3 1,'-'),(Pos 3 2,'B'),(Pos 3 3,'B'),(Pos 3 4,'-'),(Pos 3 5,'-'),(Pos 4 1,'-'),(Pos 4 2,'-'),(Pos 4 3,'B'),(Pos 4 4,'-'),(Pos 5 1,'-'),(Pos 5 2,'W'),(Pos 5 3,'-')]
testCrusher01 = crusher'_c5n7 board1 'B' 1 3 [testB12]
testCrusher02 = crusher'_c5n7 (fst testCrusher01) 'B' 2 3 [board1, testB12]
testCrusher03 = crusher'_c5n7 (fst testCrusher02) 'B' 3 3 [(fst testCrusher01), board1, testB12]


testGenUp0 = generateUps_c5n7 testB1 (Pos 1 1, 'W')
testGenUp1 = generateUps_c5n7 testB2 (Pos 1 2, 'B')
testGenUp2 = generateUps_c5n7 testB3 (Pos 2 2, 'W')
testGenUp3 = generateUps_c5n7 testB4 (Pos 4 3, 'B')
testGenUp4 = generateUps_c5n7 testB5 (Pos 4 3, 'B')
testGenUp5 = generateUps_c5n7 testB6 (Pos 4 3, 'B')
testGenUp6 = generateUps_c5n7 testB11 (Pos 2 3, 'W')

testGenDown0 = generateDowns_c5n7 testB1 (Pos 1 1, 'W')
testGenDown1 = generateDowns_c5n7 testB2 (Pos 1 2, 'B')
testGenDown2 = generateDowns_c5n7 testB3 (Pos 2 2, 'W')
testGenDown3 = generateDowns_c5n7 testB4 (Pos 4 3, 'B')
testGenDown4 = generateDowns_c5n7 testB5 (Pos 4 3, 'B')
testGenDown5 = generateDowns_c5n7 testB6 (Pos 3 3, 'B')

testGenHoriz0 = generateHorizontal_c5n7 testB1 (Pos 1 1, 'W')
testGenHoriz1 = generateHorizontal_c5n7 testB2 (Pos 1 2, 'B')
testGenHoriz2 = generateHorizontal_c5n7 testB3 (Pos 2 2, 'W')
testGenHoriz3 = generateHorizontal_c5n7 testB4 (Pos 4 3, 'B')
testGenHoriz4 = generateHorizontal_c5n7 testB5 (Pos 4 3, 'B')
testGenHoriz5 = generateHorizontal_c5n7 testB5 (Pos 4 4, 'B')

testGameOver0 = gameOver_c5n7 testB7 testGenerateBoards1
testGameOver1 = gameOver_c5n7 testB8 testGenerateBoards2
testGameOver2 = gameOver_c5n7 testB10 testGenerateBoards3

testMiniMax0 = miniMax_c5n7 0 (map (makeHeuristic_c5n7 'B') testGenerateBoards5) 

testMakeHr0 = map snd (map (makeHeuristic_c5n7 'B') testGenerateBoards5)


testCrusher14 = crusherPrint_c5n7 3 (crusher_c5n7 "WW--BB--BB---B---W-" 'B' 1 3 [])
testCrusher15 = crusherPrint_c5n7 3 (crusher_c5n7 "WW--BB--BB---B---W-" 'B' 3 3 [])
testCrusher16 = crusherPrint_c5n7 3 (crusher_c5n7 "WW--BB--BB---B---W-" 'B' 2 3 [])
testCrusher17 = crusherPrint_c5n7 3 (crusher_c5n7 "WW--BB--BB---B---W-" 'B' 4 3 [])
playCrusher22 = (playCrusher_c5n7 "WW--BB--BB---B---W-" 10 'B' 3 [])

testCrusher0 = crusher_c5n7 "www-ww-------bb-bbb" 'w' 3 3 []
testCrusher1 = crusher_c5n7 "www-ww-------bb-bbb" 'b' 3 3 []
testCrusher2 = crusher_c5n7 "WWW-WW---B---BB-BB-" 'w' 3 3 []
testCrusher3 = crusher_c5n7 "WWW-WW---B---BB-BB-" 'w' 3 3 
							["www-ww-------bb-bbb"]
testCrusher4 = crusher_c5n7 "WWWWWWWWWBBBBBBBBBB" 'w' 3 3 []
testCrusher5 = crusher_c5n7 (head testCrusher4) 'b' 3 3 (tail testCrusher4)
testCrusher6 = crusher_c5n7 "--W------WW-BWB----" 'w' 3 3 []
testCrusher7 = crusher_c5n7 "--W------WW-BWB----" 'b' 3 3 []
testCrusher8 = crusher_c5n7 "--W--W---WW-BBB----" 'W' 3 3 []
testCrusher9 = crusherPrint_c5n7 3 (crusher_c5n7 "WBWBBBB-----BBBBWBW" 'W' 3 3 [])
testCrusher10 = crusherPrint_c5n7 3 (crusher_c5n7 "-WW-W--BBB---------" 'W' 3 3 [])
testCrusher11 = crusherPrint_c5n7 3 (crusher_c5n7 "-WW-W--BBB---------" 'B' 4 3 [])
testCrusher12 = crusherPrint_c5n7 4  
			(crusher_c5n7 "WW----WB---BB------------------------" 'W' 4 4 [])
testCrusher13 = crusherPrint_c5n7 3 (crusher_c5n7 "WW--BB--BB---B---W-" 'B' 3 3 [])			

playCrusher0 = prettyPrint_c5n7 (map (splitIntoRows_c5n7 3) 
					(playCrusher_c5n7 "WWWWWWWWWBBBBBBBBBB" 7 'w' 3 []))
playCrusher1 = prettyPrint_c5n7 (map (splitIntoRows_c5n7 3) 
					(playCrusher_c5n7 "www-ww-------bb-bbb" 20 'w' 3 []))
playCrusher2 = crusherPrint_c5n7 3 (playCrusher_c5n7 "www-ww-------bb-bbb" 40 'w' 3 [])
playCrusher2a = crusherPrint_c5n7 3 (playCrusher_c5n7 "www-ww-------bb-bbb" 40 'b' 3 [])
playCrusher3 = crusherPrint_c5n7 3 (playCrusher_c5n7 "www-ww-------bb-bbb" 100 'w' 3 [])
playCrusher4 = crusherPrint_c5n7 3 (playCrusher_c5n7 "--W------WW-BWB----" 4 'w' 3 [])
playCrusher4a = crusherPrint_c5n7 3 (playCrusher_c5n7 "--W------WW-BWB----" 4 'b' 3 [])
playCrusher5 = crusherPrint_c5n7 3 (playCrusher_c5n7 "--W--W---WW-BBB----" 4 'w' 3 [])
playCrusher6 = crusherPrint_c5n7 3 (playCrusher_c5n7 "--W--W---WW-BB-B---" 4 'B' 3 [])
playCrusher7 = crusherPrint_c5n7 3 (playCrusher_c5n7 "--B--B---BB-WWW----" 4 'B' 3 [])
playCrusher8 = crusherPrint_c5n7 3 (playCrusher_c5n7 "-WW-W--BBB---------" 4 'W' 3 [])
playCrusher9 = crusherPrint_c5n7 3 (playCrusher_c5n7 "-WW-W--BBB---------" 80 'B' 3 [])
playCrusher9a = crusherPrint_c5n7 3 (playCrusher_c5n7 "-WW-W--BBB---------" 20 'B' 3 [])
playCrusher10 = crusherPrint_c5n7 4 (playCrusher_c5n7
						"WW----WBW--BBB-----------------------" 5 'W' 4 [])
playCrusher11 = crusherPrint_c5n7 4 (playCrusher_c5n7
						"WW----WBW--BBB-----------------------" 5 'B' 4 [])
playCrusher12 = crusherPrint_c5n7 4 (playCrusher_c5n7
						"WW----WBW--BBB-----------------------" 4 'W' 4 [])
playCrusher13 = crusherPrint_c5n7 4 (playCrusher_c5n7
						"WW----WBW--BBB-----------------------" 4 'B' 4 [])	
playCrusher14 = crusherPrint_c5n7 4 (playCrusher_c5n7
						"WW----WBW--BBB-----------------------" 1 'B' 4 [])							
playCrusher15 = crusherPrint_c5n7 4 (playCrusher_c5n7
						"WW----WBW--BBB-----------------------" 1 'W' 4 [])
playCrusher16 = crusherPrint_c5n7 4 (playCrusher_c5n7
						"WW----WBW--BBB-----------------------" 2 'B' 4 [])
playCrusher17 = crusherPrint_c5n7 4 (playCrusher_c5n7
						"WW----WBW--BBB-----------------------" 2 'W' 4 [])
playCrusher18 = crusherPrint_c5n7 4 (playCrusher_c5n7
						"WW----WBW--BBB-----------------------" 3 'B' 4 [])
playCrusher19 = crusherPrint_c5n7 4 (playCrusher_c5n7
						"WW----WBW--BBB-----------------------" 10 'B' 4 [])
playCrusher20 = crusherPrint_c5n7 4 (playCrusher_c5n7 
						"wwww-www---ww-----------bb---bbb-bbbb" 100 'w' 4 [])
playCrusher21 = crusherPrint_c5n7 4 (playCrusher_c5n7 
						"wwww-www---ww-----------bb---bbb-bbbb" 10 'w' 4 [])

bestMove0 = crusherPrint_c5n7 3 (getBestMove_c5n7 "-WW-W--BBB---------" 'W' 4 3 [])	
bestMove1 = crusherPrint_c5n7 3 (getBestMove_c5n7 "-WW-W--BBB---------" 'B' 4 3 [])					

playCrusher_c5n7 :: String -> Int -> Char -> Int -> [String] -> [String]						
playCrusher_c5n7 initBoard numMoves side size history 
	= playCrusherH_c5n7 initBoard 1 numMoves side size history

playCrusherH_c5n7 :: String -> Int -> Int -> Char -> Int -> [String] -> [String]
playCrusherH_c5n7 initBoard currMove numMoves side size history 
	| currMove == numMoves	= currentMove
	| otherwise 			
		= playCrusherH_c5n7	(head currentMove) 
						(currMove + 1) 
						numMoves 
						(otherSide_c5n7 side) 
						size 
						(tail currentMove)
	where currentMove = crusher_c5n7 initBoard side 3 size history 

-- Consumes a list of list of String, lolos, and prints it to the console in 
-- the format suggested by the assignment for ease of reading. If the solution 
-- is an empty list, ie the function is passed the empty list, then it prints
-- "[]". Returns a void IO value.
prettyPrint_c5n7 :: [[String]] -> IO ()
prettyPrint_c5n7 [] = putStrLn "[]"
prettyPrint_c5n7 lolos = mapM_ printStrings_c5n7 lolos

-- Consumes a list of String, los, and prints them in the requested format.
-- Adds [] characters surrounding the los, to indicate a whole board. Returns
-- a void IO value.
printStrings_c5n7 :: [String] -> IO ()
printStrings_c5n7 los = do {putStr (take ((length newString) - 1) newString); 
						putStr "\n\n"}
					where newString = unlines los 

crusherPrint_c5n7 :: Int -> [String] -> IO ()
crusherPrint_c5n7 size n = prettyPrint_c5n7 (map (splitIntoRows_c5n7 size) n)

