-- Crusher outline

type Game = (Board, Int) -- Probably change to a data
type Board = [(Position, Piece)]
data Position = Pos Int Int
				deriving (Ord, Eq, Show)
type Piece = Char

crusher_c5n7 :: [String] -> Char -> Int -> [[String]] -> [[String]]
crusher_c5n7 board side depth size history = 
	toListOfStrings ((crusher' 	(makeGame board size)  
								side 
								depth 
								(map makeGame size history)) : history)
	
crusher' :: Game -> Char -> Int -> [Game] -> Game
crusher' board _ 0 history = board
crusher' board side depth history = miniMax (crushChildren 	boardList 
															side 
															depth 
															history) 
	where boardList = generateBoards board side depth history

crushChildren :: [Game]	-> 	[Game]  
crushChildren [] _ _ _ = []
crushChildren boardList side depth history = 
	(crusher' (head boardList) side (depth - 1) (head boardList : history)) :
		   crushChildren (tail boardList)
		
generateBoards :: Game -> Char -> [Game]
generateBoards board side history = [] -- TODO
	
makeGame :: [String] -> Int -> Game
makeGame board size = getHeuristic (makeBoard board size)

getHeuristic :: Board -> Game
getHeuristic board = ([],0)

makeBoard :: [String] -> Board
makeBoard los = []

getHr :: Game -> Int
get game = fst game 

getX :: Position -> Int
getX (Pos x _) = x


-- Nadine's stuff

getY :: Position -> Int
getY (Pos _ y) = y

generateUp :: [[Char]] -> Char -> [[String]] -> Position -> [[Char]]
generateUp board side history (Pos x y)
	-- Forward Up Jump
	| (canJumpUpForward board side (Pos x y) (Pos (x - 2) (y)) history)
				= movePiece board side (Pos x y) (Pos (x - 2) (y))
	-- Backward Up Jump
	| (canJumpUpBackward board side (Pos x y) (Pos (x - 2) (y - 2)) history)
				= movePiece board side (Pos x y) (Pos (x - 2) (y - 2))
	-- Forward Up Slide
	| (canSlide board side (Pos x y) (Pos (x - 1) (y)) history)
				= movePiece board side (Pos x y) (Pos (x - 1) (y))
	-- Backward Up Slide
	| (canSlide board side (Pos x y) (Pos (x - 1) (y - 1)) history)
				= movePiece board side (Pos x y) (Pos (x - 1) (y - 1))


generateDown :: [[Char]] -> Char -> [[String]] -> Position -> [[Char]]
generateDown board side history (Pos x y)
	-- Forward Down Jump
	| (canJumpDownForward board side (Pos x y) (Pos (x + 2) (y)) history) 
				= movePiece board side (Pos x y) (Pos (x + 2) (y))
	-- Backward Down Jump
	| (canJumpDownBackward board side (Pos x y) (Pos (x + 2) (y - 2)) history) 
				= movePiece board side (Pos x y) (Pos (x + 2) (y - 2))
	-- Forward Down Slide
	| (canSlide board side (Pos x y) (Pos (x + 1) (y)) history) 
				= movePiece board side (Pos x y) (Pos (x + 1) (y))
	-- Backward Down Slide
	| (canSlide board side (Pos x y) (Pos (x + 1) (y - 1)) history) 
				= movePiece board side (Pos x y) (Pos (x + 1) (y - 1))


-- Rewritten with take and drop!
splitIntoRows_c5n7 :: [Char] -> Int -> [[Char]]
splitIntoRows_c5n7 board n
	= splitHelper_c5n7 board n n 0

splitHelper_c5n7 :: [Char] -> Int -> Int -> Int -> [[Char]]
splitHelper_c5n7 board n row_n curr
	| null board = []
	| (curr == row_n) && (curr /= 2*n - 1)
		= (take curr board) : (splitHelper_c5n7 (drop curr board) n (row_n + 1) 0)
	| (curr == 2*n - 1)
		= (take curr board) : (splitHelper_c5n7 (drop curr board) n (row_n - 1) 0)
	| otherwise
		= splitHelper_c5n7 (board) n row_n (curr + 1)

{--
boardList :: [Char] -> Int -> [[Char]]
boardList loc n =
	boardListUp loc n ((2 * n) - 1)
	
boardListUp :: [Char] -> Int -> Int -> [[Char]]
boardListUp loc current mid
	| current < mid		
		= (take current loc) : (boardListUp (drop current loc) (current + 1) mid)
	| current == mid	
		= (take current loc) : 
			(boardListDown (drop current loc) (current - 1) (getStart mid))

getStart mid = (div (mid + 1) 2)

boardListDown :: [Char] -> Int -> Int -> [[Char]]			
boardListDown loc current start
	| null loc 		= []
	| otherwise 	
		= (take current loc) : 
			(boardListDown (drop current loc) (current - 1) start)
--}

canJumpUpForward :: [[Char]] -> Char -> Position -> Position -> [[String]] -> Bool
canJumpUpForward board side from_pos to_pos history
	| ((canMakeJumpUpForward board side from_pos to_pos) == True) 
		&& ((elem (movePiece board side from_pos to_pos) history) == False) = True
	| otherwise = False

canMakeJumpUpForward :: [[Char]] -> Char -> Position -> Position -> Bool
canMakeJumpUpForward board side from_pos to_pos
	| ((isSame board side ((getX from_pos) - 1) (getY from_pos)) == True)
		&& ((isSame board side (getX to_pos) (getY to_pos)) == False)
		= True
	| otherwise = False

canJumpUpBackward :: [[Char]] -> Char -> Position -> Position -> [[String]] -> Bool
canJumpUpBackward board side from_pos to_pos history
	| ((canMakeJumpUpBackward board side from_pos to_pos) == True) 
		&& ((elem (movePiece board side from_pos to_pos) history) == False) = True
	| otherwise = False

canMakeJumpUpBackward :: [[Char]] -> Char -> Position -> Position -> Bool
canMakeJumpUpBackward board side from_pos to_pos
	| ((isSame board side ((getX from_pos) - 1) ((getY from_pos) - 1)) == True)
		&& ((isSame board side (getX to_pos) (getY to_pos)) == False)
		= True
	| otherwise = False

canJumpDownForward :: [[Char]] -> Char -> Position -> Position -> [[String]] -> Bool
canJumpDownForward board side from_pos to_pos history
	| ((canMakeJumpDownForward board side from_pos to_pos) == True) 
		&& ((elem (movePiece board side from_pos to_pos) history) == False) = True
	| otherwise = False

canMakeJumpDownForward :: [[Char]] -> Char -> Position -> Position -> Bool
canMakeJumpDownForward board side from_pos to_pos
	| ((isSame board side ((getX from_pos) + 1) (getY from_pos)) == True)
		&& ((isSame board side (getX to_pos) (getY to_pos)) == False)
		= True
	| otherwise = False

canJumpDownBackward :: [[Char]] -> Char -> Position -> Position -> [[String]] -> Bool
canJumpDownBackward board side from_pos to_pos history
	| ((canMakeJumpDownBackward board side from_pos to_pos) == True) 
		&& ((elem (movePiece board side from_pos to_pos) history) == False) = True
	| otherwise = False

canMakeJumpDownBackward :: [[Char]] -> Char -> Position -> Position -> Bool
canMakeJumpDownBackward board side from_pos to_pos
	| ((isSame board side ((getX from_pos) + 1) ((getY from_pos) - 1)) == True)
		&& ((isSame board side (getX to_pos) (getY to_pos)) == False)
		= True
	| otherwise = False

canSlide :: [[Char]] -> Char -> Position -> Position -> [[String]] -> Bool
canSlide board side from_pos to_pos history
	| ((canMakeSlide board side from_pos to_pos) == True) 
		&& ((elem (movePiece board side from_pos to_pos) history) == False) = True
	| otherwise = False

canMakeSlide :: [[Char]] -> Char -> Position -> Position -> Bool
canMakeSlide board side from_pos to_pos
	| ((isEmpty board side (getX to_pos) (getY to_pos)) == True) = True
	| otherwise = False


isSame :: [[Char]] -> Char -> Int -> Int -> Bool
isSame board side x y
	| ((getElement board x y) == side) = True
	| otherwise = False

getElement :: [[Char]] -> Int -> Int -> Char
getElement board x y
	= getElementInCol (board!!x) y 0

getElementInCol :: [Char] -> Int -> Int -> Char
getElementInCol boardRow col curr
	| null boardRow = ' '
	| (curr == col) = (head boardRow)
	| otherwise = getElementInCol (tail boardRow) col (curr + 1)


isEmpty :: [[Char]] -> Char -> Int -> Int -> Bool
isEmpty board side x y
	| ((getElement board x y) == '-') = True
	| otherwise = False



movePiece :: [[Char]] -> Char -> Position -> Position -> [[Char]]
movePiece board side from_pos to_pos
	= findElement board side 0 from_pos to_pos

findElement :: [[Char]] -> Char -> Int -> Position -> Position -> [[Char]]
findElement board side start_row from_pos to_pos
	| null board = []
	| (start_row == (getX to_pos)) 
		= (replaceElement (head board) side 0 (getY to_pos)) 
			: findElement (tail board) side (start_row + 1) from_pos to_pos
	| (start_row == (getX from_pos)) 
		= (replaceElement (head board) '-' 0 (getY from_pos))
			: findElement (tail board) side (start_row + 1) from_pos to_pos
	| otherwise = (head board) : (findElement (tail board) side (start_row + 1) from_pos to_pos)

replaceElement :: [Char] -> Char -> Int -> Int -> [Char]
replaceElement boardRow side start_col goal_col
	| null boardRow = []
	| (start_col == goal_col) = side : (tail boardRow)
	| otherwise = (head boardRow) : (replaceElement (tail boardRow) side (start_col + 1) goal_col)

