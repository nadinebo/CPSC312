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
		(crusherTop	(head (makeBoards size [board]))  
						(toUpper side) 
						depth 
						(makeBoards size history))) : history

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

crusherTop :: Board -> Char -> Int -> [Board] -> Board
crusherTop board _ 0 history = board
crusherTop board side depth history 
	| gameOver board 	= board
	| otherwise 		= 
		(fst (head (miniMax depth evaluatedBoards)))
	where 
		evaluatedBoards = 
			evaluateBoards boardList side depth history
		boardList = generateBoards board side depth history

									
crusher' :: Board -> Char -> Int -> [Board] -> Game
crusher' board side 0 history = makeHeuristic board side
crusher' board side depth history 
	| gameOver board 	= game
	| otherwise 		= 
		addHr game (getHr (head (miniMax depth evaluatedBoards)))
	where 
		game = makeHeuristic board side
		evaluatedBoards = 
			evaluateBoards	boardList side depth history
		boardList = generateBoards board side depth history

evaluateBoards :: [Board] -> Char -> Int -> [Board] -> [Game] 
evaluateBoards [] _ _ _ = []
evaluateBoards boardList side depth history = 
	(crusher' (head boardList) side (depth - 1) history) :
		   (evaluateBoards (tail boardList) side depth history)
		   
gameOver board = False -- TODO
		
generateBoards :: Board -> Char -> Int -> [Board] -> [Board]
generateBoards board side depth history 
	= (generateBoards' board history (getSide currMove board))
	where currMove = if ((mod depth 2) == 0) then (otherSide side) else side
	
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
									
slideUpLeft board (Pos row col, colour) 
	| isEmpty board newPos = 
		-- isEmpty checks if a location is '-' so also checks if it is on board
		[(replaceChars board colour (Pos row col) newPos)]
	| otherwise	= []
		where newPos = (findUpLeft board (Pos row col))
	
slideUpRight board (Pos row col, colour) 
	| isEmpty board newPos	= 
		-- isEmpty checks if a location is '-' so also checks if it is on board
		[(replaceChars board colour (Pos row col) newPos)]
	| otherwise	= []
		where newPos = (findUpRight board (Pos row col))
	
jumpUpLeft board (Pos row col, colour)
	| isGoodJump board colour jumpPos newPos
		= [(replaceChars board colour (Pos row col) newPos)]
	| otherwise = []
		where
			jumpPos = findUpLeft board (Pos row col) 
			newPos = findUpLeft board jumpPos

jumpUpRight board (Pos row col, colour)
	| isGoodJump board colour jumpPos newPos
		= [(replaceChars board colour (Pos row col) newPos)]
	| otherwise	= []
		where 
			jumpPos = findUpRight board (Pos row col) 
			newPos = findUpRight board jumpPos

slideDownLeft board (Pos row col, colour) 
	| isEmpty board newPos = 
		-- isEmpty checks if a location is '-' so also checks if it is on board
		[(replaceChars board colour (Pos row col) newPos)]
	| otherwise	= []
		where newPos = (findDownLeft board (Pos row col))
	
slideDownRight board (Pos row col, colour) 
	| isEmpty board newPos	= 
		-- isEmpty checks if a location is '-' so also checks if it is on board
		[(replaceChars board colour (Pos row col) newPos)]
	| otherwise	= []
		where newPos = (findDownRight board (Pos row col))
	
jumpDownLeft board (Pos row col, colour)
	| isGoodJump board colour jumpPos newPos
		= [(replaceChars board colour (Pos row col) newPos)]
	| otherwise = []
		where 	
			jumpPos = findDownLeft board (Pos row col) 
			newPos = findDownLeft board jumpPos

jumpDownRight board (Pos row col, colour)
	| isGoodJump board colour jumpPos newPos
		= [(replaceChars board colour (Pos row col) newPos)]
	| otherwise	= []
		where 	
			jumpPos = findDownRight board (Pos row col) 
			newPos = findDownRight board jumpPos
			
slideLeft board (Pos row col, colour) 
	| isEmpty board newPos	= 
		-- isEmpty checks if a location is '-' so also checks if it is on board
		[(replaceChars board colour (Pos row col) newPos)]
	| otherwise	= []
		where newPos = (Pos row (col - 1))
					
slideRight board (Pos row col, colour) 
	| isEmpty board newPos	= 
		-- isEmpty checks if a location is '-' so also checks if it is on board
		[(replaceChars board colour (Pos row col) newPos)]
	| otherwise	= []
		where newPos = (Pos row (col + 1))

jumpLeft board (Pos row col, colour)
	| isGoodJump board colour jumpPos newPos
		= [(replaceChars board colour (Pos row col) newPos)]
	| otherwise	= []
		where 	
			jumpPos = Pos row (col - 1) 
			newPos = Pos row (col - 2)

jumpRight board (Pos row col, colour)
	| isGoodJump board colour jumpPos newPos
		= [(replaceChars board colour (Pos row col) newPos)]
	| otherwise	= []
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
		 (makeBoard (splitIntoRows_c5n7 board size) 1 size)

makeHeuristic :: Board -> Char -> Game
makeHeuristic board side = (board, 0) --TODO

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
	if ((mod depth 2) == 0) 
		then [(minimumBy (comparing snd) logame)]
		else [(maximumBy (comparing snd) logame)]

splitIntoRows_c5n7 :: String -> Int -> [String]
splitIntoRows_c5n7 board n
	= splitHelper_c5n7 board n n 0

splitHelper_c5n7 :: String -> Int -> Int -> Int -> [String]
splitHelper_c5n7 board n row_n curr
	| null board = []
	| (curr == row_n) && (curr /= 2*n - 1)
		= (take curr board) : (splitHelper_c5n7 (drop curr board) n (row_n + 1) 0)
	| (curr == 2*n - 1)
		= (take curr board) : (splitHelper_c5n7 (drop curr board) n (row_n - 1) 0)
	| otherwise
		= splitHelper_c5n7 (board) n row_n (curr + 1)

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
testGenerateBoards0 = generateBoards (head testMakeBoards1) 'W' 1 []

testB1 = head (makeBoards 3 ["w------------------"])
testB2 = head (makeBoards 3 ["-b-----------------"])
testB3 = head (makeBoards 3 ["----w--------------"])
testB4 = head (makeBoards 3 ["--------------b----"])
testB5 = head (makeBoards 3 ["--------------bb---"])
testB6 = head (makeBoards 3 ["---------b----b----"])

testGenUp0 = generateUps testB1 (Pos 1 1, 'W')
testGenUp1 = generateUps testB2 (Pos 1 2, 'B')
testGenUp2 = generateUps testB3 (Pos 2 2, 'W')
testGenUp3 = generateUps testB4 (Pos 4 3, 'B')
testGenUp4 = generateUps testB5 (Pos 4 3, 'B')
testGenUp5 = generateUps testB6 (Pos 4 3, 'B')

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


--generateUps :: Board -> (Position, Piece) -> [Board]