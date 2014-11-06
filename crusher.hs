-- Crusher outline
import Data.Char
import Data.List
import Data.Ord

type Game = (Board, Int) -- Probably change to a data
type Board = [(Position, Piece)]
data Position = Pos Int Int
				deriving (Ord, Eq, Show)
type Piece = Char

crusher_c5n7 :: String -> Char -> Int -> Int -> [String] -> [String]
crusher_c5n7 board side depth size history = 
	(toString 
		(fst (crusher'
				(head (makeBoards size [board]))  
				(toUpper side) 
				0 
				depth 
				(makeBoards size history)
				True))) : (board : history)

-- Consumes a board and converts it to a list of String.
toString :: Board -> String
toString board = getRows board 1

-- Helper function for toListOfString. Consumes a Board, board, and an Integer, 
-- row, to convert into a string and produces a list of String by combining
-- each row together into a list.
getRows :: Board -> Int -> String
getRows board row
	| row == (getMax board) + 1	= []
	| otherwise					
		= (getString board row) ++ getRows board (row + 1)
		
-- Consumes a Board and produces the maximum column value on the board. Allows
-- getRows to terminate appropriately, and Boards of different sizes to be 
-- processed.
getMax :: Board -> Int
getMax board = maximum [n | (Pos m n, char) <- board]

-- Consumes a Board, board, and an Integer, row, and produces a String 
-- corresponding to that row in board. Collects all the character values 
-- associated with the row, row and creates a String from them.
getString :: Board -> Int -> String
getString board row = [char | (Pos m n, char) <- (sort board), m == row]
									
crusher' :: Board -> Char -> Int -> Int -> [Board] -> Bool -> Game
crusher' board side currDepth depth history isFirst
	| currDepth == depth 		= game
	| gameOver board boardList	= addHr game (gameEndScore currDepth)
	| not isFirst 
		= addHr game (getHr (head nextGame))
	| otherwise 
	= if null nextGame then game else (head nextGame)
	where 
		game = makeHeuristic board side
		evaluatedBoards = 
			evaluateBoards boardList side currDepth depth history
		boardList = generateBoards board side currDepth history
		nextGame = miniMax currDepth evaluatedBoards

evaluateBoards :: [Board] -> Char -> Int -> Int -> [Board] -> [Game] 
evaluateBoards [] _ _ _ _ = []
evaluateBoards boardList side currDepth depth history = 
	(crusher' (head boardList) side (currDepth + 1) depth history False) :
		   (evaluateBoards (tail boardList) side currDepth depth history)

gameOver :: Board -> [Board] -> Bool		   
gameOver board [] = True 
gameOver board _ = 
	(notEnoughPieces board 'W') || (notEnoughPieces board 'B') 

notEnoughPieces :: Board -> Char -> Bool
notEnoughPieces board side = (length (getSide side board)) < (getSize board)

getSize :: Board -> Int
getSize board = length [col | (Pos row col, _) <- board, row == 1] 
		
generateBoards :: Board -> Char -> Int -> [Board] -> [Board]
generateBoards board side depth history 
	= (generateBoards' board history (getSide currMove board))
	where currMove = if ((mod depth 2) == 1) then (otherSide side) else side
	
otherSide :: Char -> Char
otherSide side = if (side == 'w' || side == 'W') then 'B' else 'W'

generateBoards' :: Board -> [Board] -> [(Position, Piece)] -> [Board]	
generateBoards' _ _ [] = []
generateBoards' board history (piece:pieces) = 
	(generateBoardsFromPiece board history piece) ++ (generateBoards' board history pieces)

getSide :: Char -> Board -> [(Position, Piece)]
getSide side board = [(pos, char) | (pos, char) <- board, char == side]

generateBoardsFromPiece :: Board -> [Board] -> (Position, Piece) -> [Board]
generateBoardsFromPiece board history piece =
	filterHistory ((generateUps board piece) ++ 
					(generateDowns board piece) ++
					(generateHorizontal board piece)) history
	
filterHistory :: [Board] -> [Board] -> [Board]
filterHistory loboards history = 
	[board | board <- loboards, not (elem board history)]

generateUps :: Board -> (Position, Piece) -> [Board]	
generateUps board piece = 	(slideUpLeft board piece) ++ 
							(slideUpRight board piece) ++
							(jumpUpLeft board piece) ++
							(jumpUpRight board piece)

generateDowns :: Board -> (Position, Piece) -> [Board]
generateDowns board piece = (slideDownLeft board piece) ++
							(slideDownRight board piece) ++
							(jumpDownLeft board piece) ++
							(jumpDownRight board piece)
							
generateHorizontal :: Board -> (Position, Piece) -> [Board]							
generateHorizontal board piece = 	(slideLeft board piece) ++
									(slideRight board piece) ++
									(jumpLeft board piece) ++
									(jumpRight board piece)

doSlide board (pos, colour) newPos	
	| isEmpty board newPos = 
		-- isEmpty checks if a location is '-' so also checks if it is on board
		[(replaceChars board colour pos newPos)]
	| otherwise	= []
	
slideUpLeft board (pos, colour) = doSlide board (pos, colour) newPos
		where newPos = (findUpLeft board pos)
	
slideUpRight board (pos, colour) = doSlide board (pos, colour) newPos
		where newPos = (findUpRight board pos)

doJump board (pos, colour) jumpPos newPos 
	| isGoodJump board colour jumpPos newPos
		= [(replaceChars board colour pos newPos)]
	| otherwise = []
		
jumpUpLeft board (pos, colour) = doJump board (pos, colour) jumpPos newPos 
		where
			jumpPos = findUpLeft board pos 
			newPos = findUpLeft board jumpPos

jumpUpRight board (pos, colour) = doJump board (pos, colour) jumpPos newPos 
		where 
			jumpPos = findUpRight board pos 
			newPos = findUpRight board jumpPos

slideDownLeft board (pos, colour) = doSlide board (pos, colour) newPos
		where newPos = (findDownLeft board pos)
	
slideDownRight board (pos, colour) = doSlide board (pos, colour) newPos
		where newPos = (findDownRight board pos)
	
jumpDownLeft board (pos, colour) = doJump board (pos, colour) jumpPos newPos 
		where 	
			jumpPos = findDownLeft board pos
			newPos = findDownLeft board jumpPos

jumpDownRight board (pos, colour) = doJump board (pos, colour) jumpPos newPos 
		where 	
			jumpPos = findDownRight board pos 
			newPos = findDownRight board jumpPos
			
slideLeft board (Pos row col, colour) = 
	doSlide board (Pos row col, colour) newPos
		where newPos = (Pos row (col - 1))
					
slideRight board (Pos row col, colour) =
	doSlide board (Pos row col, colour) newPos
		where newPos = (Pos row (col + 1))

jumpLeft board (Pos row col, colour) = 
	doJump board (Pos row col, colour) jumpPos newPos 
		where 	
			jumpPos = Pos row (col - 1) 
			newPos = Pos row (col - 2)

jumpRight board (Pos row col, colour) = 
	doJump board (Pos row col, colour) jumpPos newPos 
		where 	
			jumpPos = Pos row (col + 1) 
			newPos = Pos row (col + 2)

isGoodJump :: Board -> Char -> Position -> Position -> Bool
isGoodJump board colour jumpPos newPos =
	(isSame board colour jumpPos) && (isDifferent board colour newPos)

findUpLeft board (Pos row col) 
	| row > (midRow row board)	= Pos (row - 1) col
	| otherwise 				= Pos (row - 1) (col - 1)

findUpRight board (Pos row col) 
	| row > (midRow row board)	= Pos (row - 1) (col + 1)
	| otherwise 				= Pos (row - 1) col
	
findDownLeft board (Pos row col)
	| row >= (midRow row board)	= Pos (row + 1) (col - 1)
	| otherwise 				= Pos (row + 1) col
	
findDownRight board (Pos row col)
	| row >= (midRow row board)	= Pos (row + 1) col
	| otherwise 				= Pos (row + 1) (col + 1)
		
midRow row board = head [row | ((Pos row col), _) <- board, col == maxCol]
	where maxCol = maximum [col |((Pos _ col), _) <- board] 

makeBoards :: Int -> [String] -> [Board]
makeBoards size lob = [(makeBoard' board size) | board <- lob]
	where makeBoard' board size = 
		 (makeBoard (splitIntoRows_c5n7 size board) 1 size)

makeHeuristic :: Board -> Char -> Game
makeHeuristic board side = (board, addScores board side) 

addScores board side = (winPoints board side) + (piecePoints board side)
winPoints board side 
	 | notEnoughPieces board side				= lossValue
	 | notEnoughPieces board (otherSide side) 	= winValue
	 | otherwise 								= 0

gameEndScore depth = if ((mod depth 2) == 1) then winValue else lossValue
winValue = 10
lossValue = -10

piecePoints board side = 
	(length (getSide side board)) - 
	(length (getSide (otherSide side) board))

makeBoard :: [String] -> Int -> Int -> Board
makeBoard (str:los) curr size = 
	if (curr == ((2 * size) - 1)) 
	then (makeRow str curr 1)
	else ((makeRow str curr 1) ++ (makeBoard los (curr + 1) size))

makeRow :: String -> Int -> Int -> Board
makeRow [] row col = []
makeRow (ch: loc) row col = 
	((Pos row col), (toUpper ch)) : (makeRow loc row (col + 1))

getHr :: Game -> Int
getHr game = snd game 

addHr :: Game -> Int -> Game
addHr game value = ((fst game), ((snd game) + value))

miniMax :: Int -> [Game] -> [Game]
miniMax depth [] = []
miniMax depth logame =
	if ((mod depth 2) == 1) 
		then [(minimumBy (comparing snd) logame)]
		else [(maximumBy (comparing snd) logame)]

splitIntoRows_c5n7 :: Int -> String -> [String]
splitIntoRows_c5n7 n board 
	= splitHelper_c5n7 board 1 n n

splitHelper_c5n7 :: String -> Int -> Int -> Int -> [String]
--splitHelper_c5n7 board n row_n curr
--	| null board = []
--	| (curr == row_n) && (curr < 2*n - 1)
--		= (take curr board) : (splitHelper_c5n7 (drop curr board) n (row_n + 1) 0)
--	| (curr == 2*n - 1)
--		= (take curr board) : (splitHelper_c5n7 (drop curr board) n (row_n - 1) 0)
--	| otherwise
--		= splitHelper_c5n7 (board) n row_n (curr + 1)
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
				

withinBoard :: Board -> Position -> Bool
withinBoard board pos
	= (elem True [(fst b) == pos | b <- board])

isSame :: Board -> Char -> Position -> Bool
isSame board side pos = ((getElement board pos) == [side])

isDifferent :: Board -> Char -> Position -> Bool
isDifferent board side pos 
	| (toUpper side) == 'W' 	=  elem targetPiece [['B'], ['-']]
	| (toUpper side) == 'B'		=  elem targetPiece [['W'], ['-']]
	| otherwise					= False 
		where targetPiece = getElement board pos

getElement :: Board -> Position -> [Char]
getElement board targetPos = 
	[char | (pos, char) <- board, pos == targetPos]

isEmpty :: Board -> Position -> Bool
isEmpty board pos 
	= [char | (posToCheck, char) <- board, pos == posToCheck] == ['-']

replaceChars :: Board -> Char -> Position -> Position -> Board
replaceChars board side from_pos to_pos
	= [(replace' side (pos, char)) | (pos, char) <- board]
		where replace' side (pos, char)
				| (pos == from_pos) = (pos, '-')
				| (pos == to_pos) = (pos, side)
				| otherwise = (pos, char)


testMakeBoards0 = makeBoards 2 ["-wb-wb-"]
testMakeBoards1 = makeBoards 3 ["www-ww-------bb-bbb"]
testMakeBoards2 = makeBoards 3 ["www-ww-------bb-bbb", "www-w-w------bb-bbb"]
testMakeBoards3 = makeBoards 4 ["WW----WB---BB------------------------"]

testSplitStrings = splitIntoRows_c5n7 4 "WW----WB---BB------------------------"

testB1 = head (makeBoards 3 ["w------------------"])
testB2 = head (makeBoards 3 ["-b-----------------"])
testB3 = head (makeBoards 3 ["----w--------------"])
testB4 = head (makeBoards 3 ["--------------b----"])
testB5 = head (makeBoards 3 ["--------------bb---"])
testB6 = head (makeBoards 3 ["---------b----b----"])
testB7 = head (makeBoards 3 ["WW-------B---BB-BB-"])
testB8 = head (makeBoards 3 ["WWW------B---BB-BB-"])
testB9 = head (makeBoards 3 ["WWWWWWWWWBBBBBBBBBB"])
testB10 = head (makeBoards 3 ["--W------WW-BWB----"])
testB11 = head (makeBoards 4 ["WW----WB---BB------------------------"])

testGenerateBoards0 = generateBoards (head testMakeBoards1) 'W' 1 []
testGenerateBoards1 = generateBoards testB7 'W' 1 []
testGenerateBoards2 = generateBoards testB8 'W' 1 []
testGenerateBoards3 = generateBoards testB10 'W' 1 []
testGenerateBoards4 = generateBoards testB11 'W' 1 []

testGenUp0 = generateUps testB1 (Pos 1 1, 'W')
testGenUp1 = generateUps testB2 (Pos 1 2, 'B')
testGenUp2 = generateUps testB3 (Pos 2 2, 'W')
testGenUp3 = generateUps testB4 (Pos 4 3, 'B')
testGenUp4 = generateUps testB5 (Pos 4 3, 'B')
testGenUp5 = generateUps testB6 (Pos 4 3, 'B')
testGenUp6 = generateUps testB11 (Pos 2 3, 'W')

testGenDown0 = generateDowns testB1 (Pos 1 1, 'W')
testGenDown1 = generateDowns testB2 (Pos 1 2, 'B')
testGenDown2 = generateDowns testB3 (Pos 2 2, 'W')
testGenDown3 = generateDowns testB4 (Pos 4 3, 'B')
testGenDown4 = generateDowns testB5 (Pos 4 3, 'B')
testGenDown5 = generateDowns testB6 (Pos 3 3, 'B')

testGenHoriz0 = generateHorizontal testB1 (Pos 1 1, 'W')
testGenHoriz1 = generateHorizontal testB2 (Pos 1 2, 'B')
testGenHoriz2 = generateHorizontal testB3 (Pos 2 2, 'W')
testGenHoriz3 = generateHorizontal testB4 (Pos 4 3, 'B')
testGenHoriz4 = generateHorizontal testB5 (Pos 4 3, 'B')
testGenHoriz5 = generateHorizontal testB5 (Pos 4 4, 'B')

testGameOver0 = gameOver testB7 testGenerateBoards1
testGameOver1 = gameOver testB8 testGenerateBoards2
testGameOver2 = gameOver testB10 testGenerateBoards3

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
testCrusher9 = crusherPrint 3 (crusher_c5n7 "WBWBBBB-----BBBBWBW" 'W' 3 3 [])
testCrusher10 = crusherPrint 3 (crusher_c5n7 "-WW-W--BBB---------" 'W' 3 3 [])
testCrusher11 = crusherPrint 3 (crusher_c5n7 "-WW-W--BBB---------" 'B' 4 3 [])
testCrusher12 = crusherPrint 4  
			(crusher_c5n7 "WW----WB---BB------------------------" 'W' 4 4 [])

playCrusher0 = prettyPrint (map (splitIntoRows_c5n7 3) 
					(playCrusher' "WWWWWWWWWBBBBBBBBBB" 7 'w' []))
playCrusher1 = prettyPrint (map (splitIntoRows_c5n7 3) 
					(playCrusher' "www-ww-------bb-bbb" 20 'w' []))
playCrusher2 = crusherPrint 3 (playCrusher' "www-ww-------bb-bbb" 40 'w' [])
playCrusher3 = crusherPrint 3 (playCrusher' "www-ww-------bb-bbb" 100 'w' [])
playCrusher4 = crusherPrint 3 (playCrusher' "--W------WW-BWB----" 4 'w' [])
playCrusher4a = crusherPrint 3 (playCrusher' "--W------WW-BWB----" 4 'b' [])
playCrusher5 = crusherPrint 3 (playCrusher' "--W--W---WW-BBB----" 4 'w' [])
playCrusher6 = crusherPrint 3 (playCrusher' "--W--W---WW-BB-B---" 4 'B' [])
playCrusher7 = crusherPrint 3 (playCrusher' "--B--B---BB-WWW----" 4 'B' [])
playCrusher8 = crusherPrint 3 (playCrusher' "-WW-W--BBB---------" 4 'W' [])
playCrusher9 = crusherPrint 3 (playCrusher' "-WW-W--BBB---------" 80 'B' [])
playCrusher10 = crusherPrint 4 (playCrusher
						"WW----WBW--BBB-----------------------" 5 'W' 4 [])
playCrusher11 = crusherPrint 4 (playCrusher
						"WW----WBW--BBB-----------------------" 5 'B' 4 [])
playCrusher12 = crusherPrint 4 (playCrusher
						"WW----WBW--BBB-----------------------" 4 'W' 4 [])
playCrusher13 = crusherPrint 4 (playCrusher
						"WW----WBW--BBB-----------------------" 4 'B' 4 [])	
playCrusher14 = crusherPrint 4 (playCrusher
						"WW----WBW--BBB-----------------------" 1 'B' 4 [])							
playCrusher15 = crusherPrint 4 (playCrusher
						"WW----WBW--BBB-----------------------" 1 'W' 4 [])
playCrusher16 = crusherPrint 4 (playCrusher
						"WW----WBW--BBB-----------------------" 2 'B' 4 [])
playCrusher17 = crusherPrint 4 (playCrusher
						"WW----WBW--BBB-----------------------" 2 'W' 4 [])
playCrusher18 = crusherPrint 4 (playCrusher
						"WW----WBW--BBB-----------------------" 3 'B' 4 [])
playCrusher19 = crusherPrint 4 (playCrusher
						"WW----WBW--BBB-----------------------" 10 'B' 4 [])
						
						
playCrusher initBoard numMoves side size history 
	= playCrusherH initBoard 1 numMoves side size history
	
--	| (mod numMoves 2) == 0 	
--		= playCrusherH initBoard 1 numMoves (otherSide side) size history
--	| otherwise = playCrusherOdd initBoard numMoves side size history

playCrusherH initBoard currMove numMoves side size history 
	| currMove == numMoves	= currentMove
	| otherwise 			
		= playCrusherH	(head currentMove) 
						(currMove + 1) 
						numMoves 
						(otherSide side) 
						size 
						(tail currentMove)
	where currentMove = crusher_c5n7 initBoard side 3 size history 
	
playCrusher' initBoard 1 side history = 
	crusher_c5n7 initBoard (otherSide side) 4 3 []
playCrusher' initBoard numMoves side history = 
	crusher_c5n7 (head result) (otherSide side) 4 3 (tail result)
	where result = playCrusher' initBoard (numMoves-1) (otherSide side) history

--crusher_c5n7 :: String -> Char -> Int -> Int -> [String] -> [String]
--generateUps :: Board -> (Position, Piece) -> [Board]

-- Consumes a list of list of String, lolos, and prints it to the console in 
-- the format suggested by the assignment for ease of reading. If the solution 
-- is an empty list, ie the function is passed the empty list, then it prints
-- "[]". Returns a void IO value.
prettyPrint :: [[String]] -> IO ()
prettyPrint [] = putStrLn "[]"
prettyPrint lolos = mapM_ printStrings lolos

-- Consumes a list of String, los, and prints them in the requested format.
-- Adds [] characters surrounding the los, to indicate a whole board. Returns
-- a void IO value.
printStrings :: [String] -> IO ()
printStrings los = do {putStr (take ((length newString) - 1) newString); 
						putStr "\n\n"}
					where newString = unlines los 

-- Consumes a list of String, los, and an accumulator, acc, and returns a 
-- formatted list of String. Adds appropriate ' ', '\"', and ',' characters
-- to the list of Strings to match the suggested output format.
format :: [String] -> [String] -> [String]	
format [] acc = acc
format (s:[]) acc = (acc ++ [(' ' : '\"' : s) ++ "\""])
format (s:los) [] = format los [('\"' : s) ++ "\","]
format (s:los) acc = format los (acc ++ [(" \"" ++ s) ++ "\","]) 

crusherPrint size n = prettyPrint (map (splitIntoRows_c5n7 size) n)
