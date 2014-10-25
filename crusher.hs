-- Crusher outline

type Game = (Board, Int)
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

generateUp_c5n7 :: [[Char]] -> Char -> [[String]] -> Position -> [[Char]]
generateUp_c5n7 board side history (Pos x y)
	-- Forward Up Jump
	| (canJumpUp board side (Pos (x - 2) (y)) history)
				= (movePiece board side (Pos x y) (Pos (x - 2) (y))) : history
	-- Backward Up Jump
	| (canJumpUp board side (Pos (x - 2) (y - 2)) history)
				= (movePiece board side (Pos x y) (Pos (x - 2) (y - 2))) : history
	-- Forward Up Slide
	| (canSlideUp board side (Pos (x - 1) (y)) history)
				= (movePiece board side (Pos x y) (Pos (x - 1) (y))) : history
	-- Backward Up Slide
	| (canSlideUp board side (Pos (x - 1) (y - 1)) history)
				= (movePiece board side (Pos x y) (Pos (x - 1) (y - 1))) : history	


generateDown_c5n7 :: [[Char]] -> Char -> [[String]] -> Position -> [[Char]]
generateDown_c5n7 board side history (Pos x y)
	-- Forward Down Jump
	| (canJumpDown board side (Pos (x + 2) (y)) history) 
				= (movePiece board side (Pos x y) (Pos (x + 2) (y))) : history
	-- Backward Down Jump
	| (canJumpDown board side (Pos (x + 2) (y - 2)) history) 
				= (movePiece board side (Pos x y) (Pos (x + 2) (y - 2))) : history
	-- Forward Down Slide
	| (canSlideDown board side (Pos (x + 1) (y)) history) 
				= (movePiece board side (Pos (x + 1) (y))) : history
	-- Backward Down Slide
	| (canSlideDown board side (Pos (x + 1) (y - 1)) history) 
				= (movePiece board side (Pos (x + 1) (y - 1))) : history


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

