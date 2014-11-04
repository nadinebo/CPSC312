-- Crusher outline

type Game = (Board, Int) -- Probably change to a data
type Board = [(Position, Piece)]
data Position = Pos Int Int
				deriving (Ord, Eq, Show)
type Piece = Char

crusher_c5n7 :: String -> Char -> Int -> Int -> [String] -> [String]
crusher_c5n7 board side depth size history = 
	(toListOfStrings (crusher' 	(head (makeGames size [board]))  
								side 
								depth 
								(makeGames size history))) : history
								
toListOfStrings logame = []
	
crusher' :: Game -> Char -> Int -> [Game] -> Game
crusher' board _ 0 history = board
crusher' board side depth history = miniMax (crushChildren 	boardList 
															side 
															depth 
															history) 
	where boardList = generateBoards board side depth history

crushChildren :: [Game]	-> Char -> Int -> [Game] -> [Game] 
crushChildren [] _ _ _ = []
crushChildren boardList side depth history = 
	(crusher' (head boardList) side (depth - 1) (head boardList : history)) :
		   (crushChildren (tail boardList) side depth history)
		
generateBoards :: Game -> Char -> Int -> [Game] -> [Game]
generateBoards board side depth history = [] -- TODO
	

makeGames :: Int -> [String] -> [Game]
makeGames size lob = [(makeGame board size) | board <- lob]
	where makeGame board size = 
		makeHeuristic (makeBoard (splitIntoRows_c5n7 board size) 1 size)

testMakeGames0 = makeGames 2 ["-wb-wb-"]
testMakeGames1 = makeGames 3 ["www-ww-------bb-bbb"]

makeHeuristic :: Board -> Game
makeHeuristic board = (board, 0)

makeBoard :: [String] -> Int -> Int -> Board
makeBoard (str:los) curr size = 
	if (curr == ((2 * size) - 1)) 
	then (makeRow str curr 1)
	else ((makeRow str curr 1) ++ (makeBoard los (curr + 1) size))

makeRow :: String -> Int -> Int -> Board
makeRow [] row col = []
makeRow (ch: loc) row col = ((Pos row col), ch) : (makeRow loc row (col + 1))

getHr :: Game -> Int
getHr game = snd game 

getX :: Position -> Int
getX (Pos x _) = x


-- Nadine's stuff

getY :: Position -> Int
getY (Pos _ y) = y

miniMax :: [Game] -> Game
miniMax logame = ([((Pos 0 0), '-')], 0)


splitIntoRows_c5n7 :: String -> Int -> [String]
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


--NOTES:

--For now I put heurist as 0
--For now it is returning history if no moves are available


generateUp :: Game -> Char -> [Game] -> Position -> [Game]
generateUp board side history (Pos x y)
	-- Forward Up Jump
	| (canJumpUpForward board side (Pos x y) (Pos (x - 2) (y)) history)
				= ((movePieceVertically (fst board) side (Pos x y) (Pos (x - 2) (y))), 0) : history
	-- Backward Up Jump
	| (canJumpUpBackward board side (Pos x y) (Pos (x - 2) (y - 2)) history)
				= ((movePieceVertically (fst board) side (Pos x y) (Pos (x - 2) (y - 2))), 0) : history
	-- Forward Up Slide
	| (canSlide board side (Pos x y) (Pos (x - 1) (y)) history)
				= ((movePieceHorizontally (fst board) side (Pos x y) (Pos (x - 1) (y))), 0) : history
	-- Backward Up Slide
	| (canSlide board side (Pos x y) (Pos (x - 1) (y - 1)) history)
				= ((movePieceHorizontally (fst board) side (Pos x y) (Pos (x - 1) (y - 1))), 0) : history
	| otherwise = history --for now


generateDown :: Game -> Char -> [Game] -> Position -> [Game]
generateDown board side history (Pos x y)
	-- Forward Down Jump
	| (canJumpDownForward board side (Pos x y) (Pos (x + 2) (y)) history) 
				= ((movePieceVertically (fst board) side (Pos x y) (Pos (x + 2) (y))),0) : history
	-- Backward Down Jump
	| (canJumpDownBackward board side (Pos x y) (Pos (x + 2) (y - 2)) history) 
				= ((movePieceVertically (fst board) side (Pos x y) (Pos (x + 2) (y - 2))),0) : history
	-- Forward Down Slide
	| (canSlide board side (Pos x y) (Pos (x + 1) (y)) history) 
				= ((movePieceVertically (fst board) side (Pos x y) (Pos (x + 1) (y))),0) : history
	-- Backward Down Slide
	| (canSlide board side (Pos x y) (Pos (x + 1) (y - 1)) history) 
				= ((movePieceVertically (fst board) side (Pos x y) (Pos (x + 1) (y - 1))),0) : history
	| otherwise = history


generateHorizontal :: Game -> Char -> [Game] -> Position -> [Game]
generateHorizontal board side history (Pos x y)
	-- Move Left
	| (canMoveHorizontally board side (Pos x y) (Pos x (y - 1)) history)
		= ((movePieceHorizontally (fst board) side (Pos x y) (Pos x (y - 1))),0) : history
	-- Move Right
	| (canMoveHorizontally board side (Pos x y) (Pos x (y + 1)) history)
		= ((movePieceHorizontally (fst board) side (Pos x y) (Pos x (y + 1))),0) : history
	| otherwise = history


canMoveHorizontally :: Game -> Char -> Position -> Position -> [Game] -> Bool
canMoveHorizontally board side from_pos to_pos history
	| (withinBoard board to_pos) = canMoveHorizontally' board side from_pos to_pos history
	| otherwise = False

canMoveHorizontally' :: Game -> Char -> Position -> Position -> [Game] -> Bool
canMoveHorizontally' board side from_pos to_pos history
	| (isEmpty (fst board) to_pos)
		&& ((alreadyIn (movePieceHorizontally (fst board) side from_pos to_pos) history) == False) = True
	| otherwise = False


canJumpUpForward :: Game -> Char -> Position -> Position -> [Game] -> Bool
canJumpUpForward board side from_pos to_pos history
	| (withinBoard board to_pos) = canJumpUpForward' board side from_pos to_pos history
	| otherwise = False

canJumpUpForward' :: Game -> Char -> Position -> Position -> [Game] -> Bool
canJumpUpForward' board side from_pos to_pos history
	| (canMakeJumpUpForward board side from_pos to_pos)
		&& ((alreadyIn (movePieceVertically (fst board) side from_pos to_pos) history) == False) = True
	| otherwise = False

alreadyIn :: Board -> [Game] -> Bool
alreadyIn movedBoard history
	= (elem True [(fst b) == movedBoard | b <- history])

canMakeJumpUpForward :: Game -> Char -> Position -> Position -> Bool
canMakeJumpUpForward board side from_pos to_pos
	| (isSame board side ((getX from_pos) - 1) (getY from_pos))
		&& ((isSame board side (getX to_pos) (getY to_pos)) == False)
		= True
	| otherwise = False



canJumpUpBackward :: Game -> Char -> Position -> Position -> [Game] -> Bool
canJumpUpBackward board side from_pos to_pos history
	| (withinBoard board to_pos) = canJumpUpBackward' board side from_pos to_pos history
	| otherwise = False

canJumpUpBackward' :: Game -> Char -> Position -> Position -> [Game] -> Bool
canJumpUpBackward' board side from_pos to_pos history
	| (canMakeJumpUpBackward board side from_pos to_pos) 
		&& ((alreadyIn (movePieceVertically (fst board) side from_pos to_pos) history) == False) = True
	| otherwise = False

canMakeJumpUpBackward :: Game -> Char -> Position -> Position -> Bool
canMakeJumpUpBackward board side from_pos to_pos
	| (isSame board side ((getX from_pos) - 1) ((getY from_pos) - 1))
		&& ((isSame board side (getX to_pos) (getY to_pos)) == False)
		= True
	| otherwise = False


canJumpDownForward :: Game -> Char -> Position -> Position -> [Game] -> Bool
canJumpDownForward board side from_pos to_pos history
	| (withinBoard board to_pos) = canJumpDownForward' board side from_pos to_pos history
	| otherwise = False

canJumpDownForward' :: Game -> Char -> Position -> Position -> [Game] -> Bool
canJumpDownForward' board side from_pos to_pos history
	| (canMakeJumpDownForward board side from_pos to_pos) 
		&& ((alreadyIn (movePieceVertically (fst board) side from_pos to_pos) history) == False) = True
	| otherwise = False

canMakeJumpDownForward :: Game -> Char -> Position -> Position -> Bool
canMakeJumpDownForward board side from_pos to_pos
	| (isSame board side ((getX from_pos) + 1) (getY from_pos))
		&& ((isSame board side (getX to_pos) (getY to_pos)) == False)
		= True
	| otherwise = False


canJumpDownBackward :: Game -> Char -> Position -> Position -> [Game] -> Bool
canJumpDownBackward board side from_pos to_pos history
	| (withinBoard board to_pos) = canJumpDownBackward' board side from_pos to_pos history
	| otherwise = False

canJumpDownBackward' :: Game -> Char -> Position -> Position -> [Game] -> Bool
canJumpDownBackward' board side from_pos to_pos history
	| (canMakeJumpDownBackward board side from_pos to_pos)
		&& ((alreadyIn (movePieceVertically (fst board) side from_pos to_pos) history) == False) = True
	| otherwise = False

canMakeJumpDownBackward :: Game -> Char -> Position -> Position -> Bool
canMakeJumpDownBackward board side from_pos to_pos
	| (isSame board side ((getX from_pos) + 1) ((getY from_pos) - 1))
		&& ((isSame board side (getX to_pos) (getY to_pos)) == False) = True
	| otherwise = False




canSlide :: Game -> Char -> Position -> Position -> [Game] -> Bool
canSlide board side from_pos to_pos history
	| (withinBoard board to_pos) = canSlide' board side from_pos to_pos history
	| otherwise = False

canSlide' :: Game -> Char -> Position -> Position -> [Game] -> Bool
canSlide' board side from_pos to_pos history
	| (isEmpty (fst board) to_pos)
		&& ((alreadyIn (movePieceHorizontally (fst board) side from_pos to_pos) history) == False) = True
	| otherwise = False



withinBoard :: Game -> Position -> Bool
withinBoard game pos
	= (elem True [(fst b) == pos | b <- (fst game)])


isSame :: Game -> Char -> Int -> Int -> Bool
isSame board side x y = ((getElement board x y) == [side])

getElement :: Game -> Int -> Int -> [Char]
getElement board x y = [char | (pos, char) <- (fst board), pos == (Pos x y)]


isEmpty :: Board -> Position -> Bool
isEmpty board pos 
	= [char | (posToCheck, char) <- board, pos == posToCheck] == ['-']

movePieceHorizontally :: Board -> Char -> Position -> Position -> Board
movePieceHorizontally board side from_pos to_pos
	| (isEmpty board to_pos) 
		= (replaceChars board side from_pos to_pos)
	| otherwise = board

movePieceVertically :: Board -> Char -> Position -> Position -> Board
movePieceVertically board side from_pos to_pos
	= (replaceChars board side from_pos to_pos)



replaceChars :: Board -> Char -> Position -> Position -> Board
replaceChars board side from_pos to_pos
	= [(replace' side (pos, char)) | (pos,char) <- board] 
		where replace' side (pos,char)
				| (pos == from_pos) = (pos, '-')
				| (pos == to_pos) = (pos, side)
				| otherwise = (pos,char)




-- Tests --


testgenJUF0 = generateUp ([((Pos 2 2), 'b'), ((Pos 1 2), 'b'), ((Pos 0 2), 'w')], 0) 'b' [([(Pos 2 2,'-'),(Pos 1 2,'b'),(Pos 0 2,'b')],0)] (Pos 2 2)

testgenJUF1 = generateUp ([((Pos 2 2), 'b'), ((Pos 1 2), 'b'), ((Pos 0 2), 'w')], 0) 'b' [([(Pos 2 2,'b'),(Pos 1 2,'-'),(Pos 0 2,'w')],0)] (Pos 2 2)

testgenJUB0 = generateUp ([((Pos 2 2), 'b'), ((Pos 1 1), 'b'), ((Pos 0 0), 'w')], 0) 'b' [] (Pos 2 2)

testgenJUB1 = generateUp ([((Pos 2 2), 'b'), ((Pos 1 1), 'b'), ((Pos 0 0), 'w')], 0) 'b' [([(Pos 2 2,'-'),(Pos 1 1,'b'),(Pos 0 0,'b')],0)] (Pos 2 2)

testgenJUB2 = generateUp ([((Pos 2 2), 'b'), ((Pos 1 1), 'b'), ((Pos 0 0), 'w')], 0) 'b' [([(Pos 2 2,'b'),(Pos 1 2,'-'),(Pos 0 2,'w')],0)] (Pos 2 2)

testgenSUF0 = generateUp ([((Pos 2 2), 'b'), ((Pos 1 2), '-'), ((Pos 0 2), 'w')], 0) 'b' [] (Pos 2 2)

testgenSUF1 = generateUp ([((Pos 2 2), 'b'), ((Pos 1 2), 'w'), ((Pos 0 2), 'w')], 0) 'b' [] (Pos 2 2)

testgenSUF2 = generateUp ([((Pos 2 2), 'b'), ((Pos 1 2), '-'), ((Pos 0 2), 'w')], 0) 'b' [([(Pos 2 2,'-'),(Pos 1 2,'b'),(Pos 0 2,'w')],0)] (Pos 2 2)

testgenSUB0 = generateUp ([((Pos 2 2), 'b'), ((Pos 1 1), '-'), ((Pos 0 0), 'w')], 0) 'b' [] (Pos 2 2)

testgenSUB1 = generateUp ([((Pos 2 2), 'b'), ((Pos 1 1), '-'), ((Pos 0 0), 'w')], 0) 'b' [([(Pos 2 2,'-'),(Pos 1 1,'b'),(Pos 0 0,'w')],0)] (Pos 2 2)

testgenJDF0 = generateDown ([((Pos 2 2), 'b'), ((Pos 3 2), 'b'), ((Pos 4 2), 'w')], 0) 'b' [] (Pos 2 2)

testgenJDF1 = generateDown ([((Pos 2 2), 'b'), ((Pos 3 2), '-'), ((Pos 4 2), 'w')], 0) 'b' [] (Pos 2 2)

testgenJDF2 = generateDown ([((Pos 2 2), 'b'), ((Pos 3 2), 'b'), ((Pos 4 2), 'w')], 0) 'b' [([(Pos 2 2,'-'),(Pos 3 2,'b'),(Pos 4 2,'b')],0)] (Pos 2 2)

testgenJDB0 = generateDown ([((Pos 2 2), 'b'), ((Pos 3 1), 'b'), ((Pos 4 0), 'w')], 0) 'b' [] (Pos 2 2)

testgenJDB1 = generateDown ([((Pos 2 2), 'b'), ((Pos 3 1), 'b'), ((Pos 4 0), '-')], 0) 'b' [] (Pos 2 2)

testgenJDB2 = generateDown ([((Pos 2 2), 'b'), ((Pos 3 1), '-'), ((Pos 4 0), '-')], 0) 'b' [] (Pos 2 2)

testgenJDB3 = generateDown ([((Pos 2 2), 'b'), ((Pos 3 1), 'b'), ((Pos 4 0), 'w')], 0) 'b' [([(Pos 2 2,'-'),(Pos 3 1,'b'),(Pos 4 0,'b')],0)] (Pos 2 2)

testgenSDF0 = generateDown ([((Pos 2 2), 'b'), ((Pos 3 2), '-'), ((Pos 4 0), '-')], 0) 'b' [] (Pos 2 2)
 
testgenSDF1 = generateDown ([((Pos 2 2), 'b'), ((Pos 3 2), '-'), ((Pos 4 0), '-')], 0) 'b' [([(Pos 2 2,'-'),(Pos 3 2,'b'),(Pos 4 0,'-')],0)] (Pos 2 2)

testgenSDB0 = generateDown ([((Pos 2 2), 'b'), ((Pos 3 1), '-'), ((Pos 4 0), '-')], 0) 'b' [] (Pos 2 2)

testgenSDB1 = generateDown ([((Pos 2 2), 'b'), ((Pos 3 1), '-'), ((Pos 4 0), '-')], 0) 'b' [([(Pos 2 2,'-'),(Pos 3 1,'b'),(Pos 4 0,'-')],0)] (Pos 2 2)

testgenSRH0 = generateHorizontal ([((Pos 2 2), 'b'), ((Pos 2 3), '-'), ((Pos 4 0), '-')], 0) 'b' [] (Pos 2 2)

testgenSRH1 = generateHorizontal ([((Pos 2 2), 'b'), ((Pos 2 3), 'w'), ((Pos 4 0), '-')], 0) 'b' [] (Pos 2 2)

testgenSRH2 = generateHorizontal ([((Pos 2 2), 'b'), ((Pos 2 3), '-'), ((Pos 4 0), '-')], 0) 'b' [([(Pos 2 2,'-'),(Pos 2 3,'b'),(Pos 4 0,'-')],0)] (Pos 2 2)

testgenSLH0 = generateHorizontal ([((Pos 2 2), 'b'), ((Pos 2 1), 'w'), ((Pos 4 0), '-')], 0) 'b' [] (Pos 2 2)

testgenSLH1 = generateHorizontal ([((Pos 2 2), 'b'), ((Pos 2 1), '-'), ((Pos 4 0), '-')], 0) 'b' [] (Pos 2 2)



