-- Project breakdown:

{--
* max(a,b) = -min(-a,-b) *
So maximizing player is max(a,b) and minimizing is -min(-a,-b)

Questions/Notes:
			We are only given our pawn letter as a parameter to the
			function, so do we always assume that the pawn letter 
			that is first in the alphabet goes first? 


Representation:

boardState 	is a list of tuples with actual [String] representing the 
			state, a list of its children states (moves from that state) 
			and an Int representing the heuristic for that state. 

If pawn is at location (2,2) and N = 3:

moveUpDiagonalForward 
			moves pawn to location (1,2) assuming it is free and delta = 0
			if delta = 1 then jump over peg to (0,3)


moveUpDiagonalBack 
			moves pawn to location (1,1) assuming it is free and delta = 0
			if delta = 1 then jump over peg to (0,0)

moveDownDiagonalForward 
			moves pawn to location (3,2) assuming it is free and delta = 0
			if delta = 1 then jump over peg to (4,3)

moveDownDiagonalBack 
			moves pawn to location (3,1) assuming it is free and delta = 0
			if delta = 1 then jump over peg to (4,0)

moveLeft	moves pawn to location (2,1) assuming it is free and delta = 0
			if delta = 1 then jump over peg to (2,0)


moveRight	moves pawn to location (2,3) assuming it is free and delta = 0
			if delta = 1 then jump over peg to (2,4)

--}

{-- crusher startBoard pawn depth pawnNum n
		| (pawn == maximizing pawn)
				 = getState (minimax n n (1) pawn 
					(startsearch [startBoard] pawn depth pawnNum n))
		| otherwise = getState (minimax n n (-1) pawn
					(startsearch [startBoard] pawn depth pawnNum n))


minimax give_depth start_depth player_type pawn boardStates

	if start_depth = 0
	return max (boardState)

	if start_depth = give_depth
	getHeuristic "min" pawn player_type (head boardStates)
	if player_type = -1 multiply all heuristics by -1
	go through each child and pick out whether its a loss or a tie
	by simply comparing the number of our pawns left on the board.
	The recurse back with the highest depth states chopped from the
	boardStates

	alternate state max
	getHeuristic "max" pawn player_type (head boardState)
	if player_type = -1 multiply all heuristics by -1
	go through each child with now assigned heuristics from the
	previous step and pick out the max, chop the rest and recurse
	back to this function

	alternate state min
	getHeuristic "min" pawn player_type (head boardState)
	if player_type = -1 multiply all heuristics by -1
	go through each child with now assigned heuristics from the
	previous step and pick out the min, chop the rest and recurse
	back to this function



startsearch unexplored pawn depth pawnNum n
	= generateMoves unexplored pawn depth 0 []


generateMoves boardState pawn given_depth curr_depth prevStates
	same as rush_hour, generate states based on whether the slots
	that we want to move to are free, append these states to a
	recursive call back to this function to find the other states.
	Stop when curr_depth = given_depth



moveUpDiagonalForward board pawn delta


moveUpDiagonalBack board pawn delta


moveDownDiagonalForward board pawn delta


moveDownDiagonalBack board pawn delta


moveLeft board pawn delta


moveRight board pawn delta


min


max

-- player_type is -1 or 1, depending on the player we are playing with

getHeuristic operation pawn player_type boardState
		if operation = "max"
			returns max heuristic of the state * player_type

		if operation = "min"
			returns min heuristic of the state + player_type


--}