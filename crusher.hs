-- Crusher --
import Data.Char
import Data.List
import Data.Ord

type Game = (Board, Int)
type Board = [(Position, Piece)]
data Position = Pos Int Int
				deriving (Ord, Eq, Show)
type Piece = Char

-- Main crusher function
-- Consumes a list of strings representing the board, a side for which to generate
-- moves for, depth until which the moves are to be generated, and a size of each
-- board side. Note: the board is a hexagon.
-- Parameters: 	(board:history) = boards already played with last one being the one
--				we are to continue playing from
--				side = the side that we are playing for
--				depth = the depth until which to look for best move
--				size = the size of each side of the hexagonal board
-- Returns:		a list containing (board:history) and the next move that has the
--				best heuristic for winning until this depth

crusher_c5n7 :: [String] -> Char -> Int -> Int -> [String]
crusher_c5n7 (board:history) side depth size = 
	(toString_c5n7 
		(fst (fst (crusher'_c5n7
				(head (makeBoards_c5n7 size [board]))  
				(toUpper side) 
				0 
				depth 
				(makeBoards_c5n7 size history))))) : (board : history)

-- Evaluates the board's heuristic value by applying miniMax algorithm to
-- its children states	and returns the game with the most optimal heuristic
-- for the given side.
-- Parameters:	board = Board representation
--				side = board side that we are playing as
--				currDepth = the current depth of our board state
--				depth = the depth until which we are generating states (given)
--				history = passed board states generated since the match began
-- Returns:		a game which has the most optimal heuristic value for the given
--				side (i.e., the 'best move')

crusher'_c5n7 :: Board -> Char -> Int -> Int -> [Board] -> (Game, Int)
crusher'_c5n7 board side currDepth depth history
	| currDepth == depth	= (game, depth)
	| gameOver_c5n7 board generatedBoards
							= ((board, (gameEndScore_c5n7 currDepth)), 0)
	| currDepth == 0		= miniMax_c5n7 currDepth evaluatedChildren
	| otherwise 			= ((board, (snd (fst nextGame))), (snd nextGame) + 1)
	where 
		game = makeHeuristic_c5n7 side board
		nextGame = miniMax_c5n7 currDepth evaluatedChildren
		evaluatedChildren = runCrusherOnEach_c5n7 	generatedBoards 
												side 
												(currDepth + 1) 
												depth 
												(board:history)
		generatedBoards = generateBoards_c5n7 board side currDepth history

-- Alternates taking the min and max values of heuristics between the depth
-- levels of 'children' games to help derive the best next move based on the
-- games generated for the original board.
-- Parameters:	logame = list of games generated
--				depth = the current depth
-- Returns:		a game and heuristic for it

miniMax_c5n7 :: Int -> [(Game, Int)] -> (Game, Int)
miniMax_c5n7 depth logame =
	if ((mod depth 2) == 1) 
		then minimumBy (comparing snd) allMins
		else minimumBy (comparing snd) allMaxes
	where
		minHr = snd (minimumBy (comparing snd) (map fst logame))
		allMins = [(game, moves)|(game, moves) <- logame, (snd game) == minHr]
		maxHr = snd (maximumBy (comparing snd) (map fst logame))
		allMaxes = [(game, moves)|(game, moves) <- logame, (snd game) == maxHr]

-- Calls the crusher function on each of the children of a game state.
-- Parameters: (board:boards) = list of boards to run
--				side = board side that we are playing as
--				currDepth = the current depth of our board state
--				depth = the depth until which we are generating states (given)
--				history = passed board states generated since the match began
-- Returns:		a list of games with their heuristics

runCrusherOnEach_c5n7 :: [Board] -> Char -> Int -> Int -> [Board] -> [(Game, Int)]
runCrusherOnEach_c5n7 [] _ _ _ _ = []
runCrusherOnEach_c5n7 (board:boards) side currDepth depth history =
	(crusher'_c5n7 board side currDepth depth history) :
				(runCrusherOnEach_c5n7 boards side currDepth depth history)

-- Converts board data structure to a list of String.
-- Parameters: 	board = latest board state move
-- Returns:		a string created from concatenating our board state

toString_c5n7 :: Board -> String
toString_c5n7 board = getRows_c5n7 board 1

-- Helper function for toString_c5n7. 
-- Parameters :	board = Board repsentation 
-- 				row = row to convert into a string
-- Returns:		a list of String by combining each row together into a list.

getRows_c5n7 :: Board -> Int -> String
getRows_c5n7 board row
	| row == (getMax_c5n7 board) + 1	= []
	| otherwise					
		= (getString_c5n7 board row) ++ getRows_c5n7 board (row + 1)
		
-- Finds the maximum column value on the board. Allows getRows to terminate 
-- appropriately, and Boards of different sizes to be.
-- processed.
-- Parameters: 	board = Board representation
-- Returns:		maximum column value

getMax_c5n7 :: Board -> Int
getMax_c5n7 board = maximum [n | (Pos m n, char) <- board]

-- Collects all the character values associated with the row and creates 
-- a String from them.
-- Parameters:	board = Board representation
--				row = row we are currently working with
-- Returns:		produces a string corresponding to that row

getString_c5n7 :: Board -> Int -> String
getString_c5n7 board row = [char | (Pos m n, char) <- (sort board), m == row]
		
-- Determines whether there are no more moves to make either because there are
-- no generated boards or not enough pieces from either of the sides.
-- Parameters:	board = Board representation
--				second argument = generated boards
-- Returns:		True or False depending on if there are any moves left

gameOver_c5n7 :: Board -> [Board] -> Bool		   
gameOver_c5n7 board [] = True 
gameOver_c5n7 board _ = 
	(notEnoughPieces_c5n7 board 'W') || (notEnoughPieces_c5n7 board 'B') 

-- Determines if the number of pieces of the given side is less than the size
-- of the board (the original one) and and if so it returns True.
-- Parameters:	board = Board representation
--				side = board side that we are playing as
-- Returns:		True or False depending on if # pieces < board size

notEnoughPieces_c5n7 :: Board -> Char -> Bool
notEnoughPieces_c5n7 board side = 
	(length (getSide_c5n7 side board)) < (getSize_c5n7 board)

-- Returns the original size of the board we are given by determining the size
-- of the very first row.
-- Parameters:	board = Board representation
-- Returns:		the size of each board side (board is a hexagon)

getSize_c5n7 :: Board -> Int
getSize_c5n7 board = length [col | (Pos row col, _) <- board, row == 1] 

-- Generates new board states from a given board.
-- Parameters:	board = Board representation
--				side = the side that we want to generate states for
--				depth = the depth until which we are generating states (given)
--				history = passed board states generated since the match began
-- Returns:		a list of board states generated

generateBoards_c5n7 :: Board -> Char -> Int -> [Board] -> [Board]
generateBoards_c5n7 board side depth history 
	= (generateBoards'_c5n7 board history (getSide_c5n7 currMove board))
	where currMove = if ((mod depth 2) == 1) then (otherSide_c5n7 side) else side
	
-- Returns the opposite side to the one we provide.
-- Parameters:	side = start side
-- Returns: 	the side's opponent

otherSide_c5n7 :: Char -> Char
otherSide_c5n7 side = if (side == 'w' || side == 'W') then 'B' else 'W'

-- Generates the board states for each of the board pieces.
-- Parameters: 	board = Board representation (current board)
--				history = passed board states generated since the match began
--				(piece:pieces) = list of board pieces
-- Returns:		the states generated for each of the pieces

generateBoards'_c5n7 :: Board -> [Board] -> [(Position, Piece)] -> [Board]	
generateBoards'_c5n7 _ _ [] = []
generateBoards'_c5n7 board history (piece:pieces) = 
	(generateBoardsFromPiece_c5n7 board history piece) ++ 
	(generateBoards'_c5n7 board history pieces)

-- Finds and returns all the Boards (list of positions and pieces) for the side
-- Parameters:	side = the side we are working with
--				board = Board representation
-- Returns:		all the Board parts for the side

getSide_c5n7 :: Char -> Board -> [(Position, Piece)]
getSide_c5n7 side board = [(pos, char) | (pos, char) <- board, char == side]

-- Given a piece, finds the boards that could be generated though moving this piece
-- that are not part of the history (unique boards)
-- Parameters:	board = Board representation
--				history = passed board states generated since the match began
--				piece = side we are working with
-- Returns:		boards that could be generated through moving this piece

generateBoardsFromPiece_c5n7 :: Board -> [Board] -> (Position, Piece) -> [Board]
generateBoardsFromPiece_c5n7 board history piece =
	filterHistory_c5n7 ((generateUps_c5n7 board piece) ++ 
					(generateDowns_c5n7 board piece) ++
					(generateHorizontal_c5n7 board piece)) history

-- Filters the move history such that we don't have any any circular board states
-- (i.e., we don't return to the same board state again).
-- Parameters:	loboards = list of boards generated
--				history = board states generated before
-- Returns:		list of boards that don't have duplicate states that we generated
--				already previously

filterHistory_c5n7 :: [Board] -> [Board] -> [Board]
filterHistory_c5n7 loboards history = 
	[board | board <- loboards, not (elem board history)]

-- Generates an upward movement for a piece in the board.
-- Parameters:	board = Board representation of the state we are working with
--				piece = the side that we are working with
-- Returns:		list of Boards that represents moves upward for the piece
generateUps_c5n7 :: Board -> (Position, Piece) -> [Board]	
generateUps_c5n7 board piece = (slideUpLeft_c5n7 board piece) ++ 
							(slideUpRight_c5n7 board piece) ++
							(jumpUpLeft_c5n7 board piece) ++
							(jumpUpRight_c5n7 board piece)

-- Generates a downward movement for a piece in the board.
-- Parameters:	board = Board representation of the state we are working with
--				piece = the side that we are working with
-- Returns:		list of Boards that represents moves downward for the piece
generateDowns_c5n7 :: Board -> (Position, Piece) -> [Board]
generateDowns_c5n7 board piece = (slideDownLeft_c5n7 board piece) ++
							(slideDownRight_c5n7 board piece) ++
							(jumpDownLeft_c5n7 board piece) ++
							(jumpDownRight_c5n7 board piece)
	
-- Generates a horizontal movement for a piece in the board.
-- Parameters:	board = Board representation of the state we are working with
--				piece = the side that we are working with
-- Returns:		list of Boards that represents the horizontal moves for the piece						
generateHorizontal_c5n7 :: Board -> (Position, Piece) -> [Board]							
generateHorizontal_c5n7 board piece = (slideLeft_c5n7 board piece) ++
									(slideRight_c5n7 board piece) ++
									(jumpLeft_c5n7 board piece) ++
									(jumpRight_c5n7 board piece)

-- Slides a piece to an empty spot thats adjecent to the board piece in any of
-- the directions.
-- Parameters:	board = Board representation (current board)
--				(pos, colour) = moving the piece 'colour' from position pos
--				newPos = the position we intent to move the piece to
-- Returns:		a list of board states with the piece being moved to newPos

doSlide_c5n7 :: Board -> (Position, Piece) -> Position -> [Board]
doSlide_c5n7 board (pos, colour) newPos	
	| isEmpty_c5n7 board newPos = 
		-- isEmpty checks if a location is '-' so also checks if it is on board
		[(replaceChars_c5n7 board colour pos newPos)]
	| otherwise	= []

-- slideUpLeft_c5n7 and slideUpRight_c5n7:
-- Slide the piece up and left/right from the position that it is currently in.
-- Parameters:	board = Board representation (current board)
--				(pos, colour) = moving the piece 'colour' from position pos
-- Returns:		a list of board states with the piece being moved to the
--				desired position (up and left/right)

slideUpLeft_c5n7 :: Board -> (Position, Piece) -> [Board]	
slideUpLeft_c5n7 board (pos, colour) = doSlide_c5n7 board (pos, colour) newPos
		where newPos = (findUpLeft_c5n7 board pos)

slideUpRight_c5n7 :: Board -> (Position, Piece) -> [Board] 	
slideUpRight_c5n7 board (pos, colour) = doSlide_c5n7 board (pos, colour) newPos
		where newPos = (findUpRight_c5n7 board pos)

-- Leap the piece over to the desired position. This function is used to capture
-- opponent's pieces.
-- Parameters:	board = Board representation (current board)
--				(pos, colour) = moving the piece 'colour' from position pos
--				jumpPos = the appropiate board position to jump to for this piece
--						  (it is in between the piece and the opponent's piece)
--				newPos = position to jump to (adjecent to jumpPos)
-- Returns:		a list of board states with the piece being moved to the
--				desired position (leap over a piece)

doJump_c5n7 :: Board -> (Position, Piece) -> Position -> Position -> [Board] 
doJump_c5n7 board (pos, colour) jumpPos newPos 
	| isGoodJump_c5n7 board colour jumpPos newPos
		= [(replaceChars_c5n7 board colour pos newPos)]
	| otherwise = []

-- jumpUpLeft_c5n7 and jumpUpRight_c5n7:
-- Leap the piece over its own diagonally adjecent piece. Also used to capture
-- opponent's piece that are up. It determines the apporiate spot to jump
-- to by defining jumpPos and newPos.
-- Parameters:	board = Board representation (current board)
-- 				(pos, colour) = moving the piece 'colour' from position pos
-- Returns:		a list of board states with the piece being moved to the
--				desired position (leap over a piece up)

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

-- slideDownLeft_c5n7 and slideDownRight_c5n7:
-- Slide the piece down and left/right from the position that it is currently in.
-- Parameters:	board = Board representation (current board)
--				(pos, colour) = moving the piece 'colour' from position pos
-- Returns:		a list of board states with the piece being moved to the
--				desired position (down and left/right)

slideDownLeft_c5n7 :: Board -> (Position, Piece) -> [Board] 
slideDownLeft_c5n7 board (pos, colour) = doSlide_c5n7 board (pos, colour) newPos
		where newPos = (findDownLeft_c5n7 board pos)
	
slideDownRight_c5n7 :: Board -> (Position, Piece) -> [Board] 
slideDownRight_c5n7 board (pos, colour) = doSlide_c5n7 board (pos, colour) newPos
		where newPos = (findDownRight_c5n7 board pos)

-- jumpDownLeft_c5n7 and jumpDownRight_c5n7:
-- Leap the piece over its own diagonally adjecent piece. Also used to capture
-- opponent's piece that are down. It determines the apporiate spot to jump
-- to by defining jumpPos and newPos.
-- Parameters:	board = Board representation (current board)
-- 				(pos, colour) = moving the piece 'colour' from position pos
-- Returns:		a list of board states with the piece being moved to the
--				desired position (leap over a piece down left/right)

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


-- slideLeft_c5n7 and slideRight_c5n7:
-- Determine a new position for the piece to slide to based on it's location.
-- Parameters:	board = Board representation (current board)
--				(Pos row col, colour) = the position and colour of the piece to be
--				moved at that position (side)
-- Returns:		a list of board states generated from doSlide_c5n7

slideLeft_c5n7 :: Board -> (Position, Piece) -> [Board] 			
slideLeft_c5n7 board (Pos row col, colour) = 
	doSlide_c5n7 board (Pos row col, colour) newPos
		where newPos = (Pos row (col - 1))
	
slideRight_c5n7 :: Board -> (Position, Piece) -> [Board] 				
slideRight_c5n7 board (Pos row col, colour) =
	doSlide_c5n7 board (Pos row col, colour) newPos
		where newPos = (Pos row (col + 1))


-- jumpLeft_c5n7 and jumpRight_c5n7:
-- Determine a new position for the piece to jump to based on it's location.
-- The jump position location deffers based on where the piece is on the board
-- (the first or the second half vs. middle)
-- Parameters:	board = Board representation (current board)
--				(Pos row col, colour) = the position and colour of the piece to be
--				moved at that position (side)
-- Returns:		a list of board states generated from doSlide_c5n7

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


-- Determines if the jump is legal: checks if the adjecent piece is of the same
-- colour and that the piece after it (to where we want to jump) is of a different
-- colour (so we can crush it).
-- Parameters:	board = Board representation (current board)
-- 				colour = piece colour (side)
--				jumpPos = position adjecent to our piece
--				newPos = position we want to jump to
-- Returns:		True or False

isGoodJump_c5n7 :: Board -> Char -> Position -> Position -> Bool
isGoodJump_c5n7 board colour jumpPos newPos =
	(isSame_c5n7 board colour jumpPos) && (isDifferent_c5n7 board colour newPos)

-- findUpLeft_c5n7, findUpRight_c5n7, findDownLeft_c5n7 and findDownRight_c5n7:
-- Finds the correct position to move to for the piece. This is because the
-- the location of the piece changes the delta between the rows/cols for the jumps.
-- Parameters:	board = Board representation (current board)
--				(Pos row col) = current position we are considering
-- Returns:		the correct board position we can move to

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

-- Finds the middlemost row of the board (it has the most columns).
-- Parameters:	row = current row
--				board = Board representation (current board)
-- Returns:		the row number that is the middle most row

midRow_c5n7 :: Int -> Board -> Int		
midRow_c5n7 row board = head [row | ((Pos row col), _) <- board, col == maxCol]
	where maxCol = maximum [col |((Pos _ col), _) <- board] 

-- Turns a string input of boards (given) into our Board representation
-- Parameters:	size = size of each hexagonal side (given)
--				lob = list of strings representing boards (given)
-- Returns:		our Board representation of the board (splits into positions and
--				and piece values)

makeBoards_c5n7 :: Int -> [String] -> [Board]
makeBoards_c5n7 size lob = [(makeBoard' board size) | board <- lob]
	where makeBoard' board size = 
		 (makeBoard_c5n7 (splitIntoRows_c5n7 size board) 1 size)

-- Generates a heuristic for the board.
-- Parameters:	side = side we are working with
--				board = Board representation (current board)
-- Returns:		a game which contains the board and the heuristic for it

makeHeuristic_c5n7 :: Char -> Board -> Game
makeHeuristic_c5n7 side board = (board, addScores_c5n7 board side) 

-- Adds all the scores for the given board state.
-- Parameters:	side = side we are working with
--				board = Board representation (current board)
-- Returns:		a score for that board and side

addScores_c5n7 :: Board -> Char -> Int
addScores_c5n7 board side 
	| win /= 0		= win
	| otherwise		= piecePoints_c5n7 board side
	where win = (winPoints_c5n7 board side)


-- Assigns points for a given board configuration. Works with addScores_c5n7
-- in order to help generate the correct board heuristic.
-- Parameters:	side = side we are working with
--				board = Board representation (current board)
-- Returns:		an int representing a win or loss value (or 0 if neither)

winPoints_c5n7 :: Board -> Char -> Int
winPoints_c5n7 board side 
	 | notEnoughPieces_c5n7 board side					= lossValue_c5n7
	 | notEnoughPieces_c5n7 board (otherSide_c5n7 side)	= winValue_c5n7
	 | otherwise 									= 0

-- Determines whether the given depth was our move or opponents and assigns a 
-- winning or a loosing value to the game accordingly.
-- Parameters:	depth = given depth
-- Returns:		a winning or a loosing value for the game

gameEndScore_c5n7 :: Int -> Int
gameEndScore_c5n7 depth = if ((mod depth 2) == 1) 
							then winValue_c5n7 
							else lossValue_c5n7

-- Constant values for heuristics:
winValue_c5n7 = 10
lossValue_c5n7 = -10

-- Determines how many piece difference there is between the boards. The difference
-- is either positive or negative depending on which side we play.
-- Parameters:	side = side we are working with
--				board = Board representation (current board)
-- Returns:		piece point difference

piecePoints_c5n7 :: Board -> Char -> Int
piecePoints_c5n7 board side = 
	(length (getSide_c5n7 side board)) - 
	(length (getSide_c5n7 (otherSide_c5n7 side) board))

-- Turns a list of strings into a Board representation for a given row.
-- Parameters:	(str:los) = list of strings representing the board (given)
--				 curr = current row we're considering
--				size = size of the board (given)
-- Returns:		a Board representation

makeBoard_c5n7 :: [String] -> Int -> Int -> Board
makeBoard_c5n7 (str:los) curr size = 
	if (curr == ((2 * size) - 1)) 
	then (makeRow_c5n7 str curr 1)
	else ((makeRow_c5n7 str curr 1) ++ (makeBoard_c5n7 los (curr + 1) size))

-- Helper for makeBoard_c5n7. Turns each row into a Board type representation.
-- Parameters:	(ch:los) = a string representing the board row
--				row = current row
--				col = current col
-- Returns:		a Board representation

makeRow_c5n7 :: String -> Int -> Int -> Board
makeRow_c5n7 [] row col = []
makeRow_c5n7 (ch: loc) row col = 
	((Pos row col), (toUpper ch)) : (makeRow_c5n7 loc row (col + 1))

-- Splits the given string into rows that match the haxagonal shape. Each side of
-- the hexagonal board is n characters long.
-- Parameters:	board = string representation of the board (given)
--				n = size of the board (given)
-- Returns:		a list of strings each representing a row in the board with size = n

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

-- Checks if the piece at a given position is the same as the given piece.
-- Parameters:	side = side we are working with
--				board = Board representation (current board)
--				pos = position to check
-- Returns:		True or False

isSame_c5n7 :: Board -> Char -> Position -> Bool
isSame_c5n7 board side pos = ((getElement_c5n7 board pos) == [side])

-- Checks if the piece at a given position is different than the given piece.
-- Note that an empty piece is considered a different piece.
-- Parameters:	side = side we are working with
--				board = Board representation (current board)
--				pos = position to check
-- Returns:		True or False

isDifferent_c5n7 :: Board -> Char -> Position -> Bool
isDifferent_c5n7 board side pos 
	| (toUpper side) == 'W' 	=  elem targetPiece [['B'], ['-']]
	| (toUpper side) == 'B'		=  elem targetPiece [['W'], ['-']]
	| otherwise					= False 
		where targetPiece = getElement_c5n7 board pos

-- Returns an element at the given position for the given board.
-- Parameters:	board = Board representation (current board)
--				targetPos = position to check
-- Returns:		element at targetPos

getElement_c5n7 :: Board -> Position -> [Char]
getElement_c5n7 board targetPos = 
	[char | (pos, char) <- board, pos == targetPos]

-- Checks if the given position is empty.
-- Parameters:	board = Board representation (current board)
--				pos = position to check
-- Returns:		True or False

isEmpty_c5n7 :: Board -> Position -> Bool
isEmpty_c5n7 board pos 
	= [char | (posToCheck, char) <- board, pos == posToCheck] == ['-']

-- Replaces a character in to_pos with the given character and replaces the
-- from_pos (the original position of the character) with an empty slot ('-').
-- This similulates the character moving across the board.
-- Parameters:	side = the character we want to place at to_pos
--				board = Board representation (current board)
--				to_pos = position to move the character to
--				from_pos = position that side is moving from (will be empty)
-- Returns:		the same board with the character side moved to the desired
--				position and an empty slot in place of its old position.

replaceChars_c5n7 :: Board -> Char -> Position -> Position -> Board
replaceChars_c5n7 board side from_pos to_pos
	= [(replace' side (pos, char)) | (pos, char) <- board]
		where replace' side (pos, char)
				| (pos == from_pos) = (pos, '-')
				| (pos == to_pos) = (pos, side)
				| otherwise = (pos, char)

-- The End.



-------------------------------- TESTS -------------------------------------
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

testCrushOnEach0 = runCrusherOnEach_c5n7 testGenerateBoards5 'B' 1 3 [testB12]
testCrushOnEach1 = runCrusherOnEach_c5n7 testGenerateBoards5 'B' 1 4 [testB12]
board1 = 	[(Pos 1 1,'W'),(Pos 1 2,'W'),(Pos 1 3,'-'),
		(Pos 2 1,'-'),(Pos 2 2,'B'),(Pos 2 3,'B'),(Pos 2 4,'-'),
	(Pos 3 1,'-'),(Pos 3 2,'B'),(Pos 3 3,'B'),(Pos 3 4,'-'),(Pos 3 5,'-'),
		(Pos 4 1,'-'),(Pos 4 2,'-'),(Pos 4 3,'B'),(Pos 4 4,'-'),
			(Pos 5 1,'-'),(Pos 5 2,'W'),(Pos 5 3,'-')]
testCrusher01 = crusher'_c5n7 board1 'B' 1 3 [testB12]
testCrusher02 = crusher'_c5n7 (fst (fst testCrusher01)) 'B' 2 3 [board1, testB12]
testCrusher03 = crusher'_c5n7 (fst (fst testCrusher02)) 'B' 3 3 
					[(fst (fst testCrusher01)), board1, testB12]

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

testMakeHr0 = map snd (map (makeHeuristic_c5n7 'B') testGenerateBoards5)

testCrusher14 = crusherPrint_c5n7 3 (crusher_c5n7 ["WW--BB--BB---B---W-"] 'B' 1 3)
testCrusher15 = crusherPrint_c5n7 3 (crusher_c5n7 ["WW--BB--BB---B---W-"] 'B' 3 3)
testCrusher16 = crusherPrint_c5n7 3 (crusher_c5n7 ["WW--BB--BB---B---W-"] 'B' 2 3)
testCrusher17 = crusherPrint_c5n7 3 (crusher_c5n7 ["WW--BB--BB---B---W-"] 'B' 4 3)
playCrusher22 = (playCrusher_c5n7 "WW--BB--BB---B---W-" 10 'B' 3 [])

testCrusher0 = crusher_c5n7 ["www-ww-------bb-bbb"] 'w' 3 3 
testCrusher1 = crusher_c5n7 ["www-ww-------bb-bbb"]'b' 3 3 
testCrusher2 = crusher_c5n7 ["WWW-WW---B---BB-BB-"] 'w' 3 3 
testCrusher3 = crusher_c5n7 ["WWW-WW---B---BB-BB-","www-ww-------bb-bbb"] 'w' 3 3 
testCrusher4 = crusher_c5n7 ["WWWWWWWWWBBBBBBBBBB"] 'w' 3 3
testCrusher5 = crusher_c5n7 testCrusher4 'b' 3 3
testCrusher6 = crusher_c5n7 ["--W------WW-BWB----"] 'w' 3 3
testCrusher7 = crusher_c5n7 ["--W------WW-BWB----"] 'b' 3 3
testCrusher8 = crusher_c5n7 ["--W--W---WW-BBB----"] 'W' 3 3
testCrusher9 = crusherPrint_c5n7 3 (crusher_c5n7 ["WBWBBBB-----BBBBWBW"] 'W' 3 3)
testCrusher10 = crusherPrint_c5n7 3 (crusher_c5n7 ["-WW-W--BBB---------"] 'W' 3 3)
testCrusher11 = crusherPrint_c5n7 3 (crusher_c5n7 ["-WW-W--BBB---------"] 'B' 4 3)
testCrusher12 = crusherPrint_c5n7 4  
			(crusher_c5n7 ["WW----WB---BB------------------------"] 'W' 4 4)
testCrusher13 = crusherPrint_c5n7 3 (crusher_c5n7 ["WW--BB--BB---B---W-"] 'B' 3 3)			

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

-------------------- GAME PLAYING FUNCTIONS FOR TESTING ----------------------
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
	where currentMove = crusher_c5n7 (initBoard:history) side 3 size

-------------------------- HOMEMADE PRETTY PRINTS -----------------------------

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