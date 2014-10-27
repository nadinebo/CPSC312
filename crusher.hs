-- Crusher outline

type Game = (Board, Int) -- Probably change to a data
type Board = [(Position, Piece)]
data Position = Pos Int Int
				deriving (Ord, Eq, Show)
type Piece = Char

crusher :: [String] -> Int -> Char -> Int -> [[String]] -> [[String]]
crusher board size side depth history = 
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
