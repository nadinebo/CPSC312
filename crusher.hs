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
generateUp_c5n7 :: Game -> Char -> [Game] -> Pos -> Game 
generateUp_c5n7 board side history (Pos x y)
	-- Forward Up Jump
	| (canJumpUp board side (Pos (x - 2) (y + 1)) history)
				= jumpUp board side (Pos x y) (Pos (x - 2) (y + 1))
	-- Backward Up Jump
	| (canJumpUp board side (Pos (x - 2) (y - 1)) history)
				= jumpUp board side (Pos x y) (Pos (x - 2) (y - 1))
	-- Forward Up Slide
	| (canSlideUp board side (Pos (x - 1) (y)) history)
				= slideUp board side (Pos x y) (Pos (x - 1) (y))
	-- Backward Up Slide
	| (canSlideUp board side (Pos (x - 1) (y - 1)) history)
				= slideUp board side (Pos x y) (Pos (x - 1) (y - 1))	


generateDown_c5n7 :: Game -> Char -> [Game] -> Pos -> Game
generateDown_c5n7 board side history (Pos x y)
	-- Forward Down Jump
	| (canJumpDown board side (Pos (x + 2) (y + 1)) history) 
				= jumpDown board side (Pos x y) (Pos (x + 2) (y + 1))
	-- Backward Down Jump
	| (canJumpDown board side (Pos (x + 2) (y - 2)) history) 
				= jumpDown board side (Pos x y) (Pos (x + 2) (y - 2))
	-- Forward Down Slide
	| (canSlideDown board side (Pos (x + 1) (y)) history) 
				= slideDown board side (Pos (x + 1) (y))
	-- Backward Down Slide
	| (canSlideDown board side (Pos (x + 1) (y - 1)) history) 
				= slideDown board side (Pos (x + 1) (y - 1))



splitByRows :: String -> Int -> [[Char]]
splitByRows board n
	= getBoardRow board n n 0 "" []

getBoardRow :: [Char] -> Int -> Int -> Int -> [Char] -> [[Char]] -> [[Char]]
getBoardRow board original_n new_n curr row_content split_board
	| null board = reverse(reverse(row_content) : split_board)
	| (curr == new_n) && (curr /= (2*original_n - 1))
		= getBoardRow (board) original_n (new_n+1) (0) [] (reverse(row_content) : split_board) 
	| (curr == (2*original_n - 1))
		= getBoardRow (board) original_n (new_n-1) (0) [] (reverse(row_content) : split_board)
	| otherwise = getBoardRow 
				(tail board) original_n new_n (curr+1) ((head board) : row_content) split_board


