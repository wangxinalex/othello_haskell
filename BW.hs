module BW where
data Piece = White | Black | Empty deriving (Eq, Show)
type Position = (Int, Int)
type ZBoard = [(Position, Piece)]

type Board = [Piece]
type Step = (Position, Piece) -- ((Int, Int), Piece)

-- initial a board of size n with four pieces occupied
initialZBoard:: Int -> ZBoard
initialZBoard size = [((x, y), Empty) | x <- [1..size `div` 2-1], y <- [1..size]]
					++ [((x, y), Empty) | x <- [size `div` 2+2..size], y <- [1..size]]
					++ [((x, y), Empty) | x <- [2..(size-1)], y <- [size `div` 2+2..size]]
					++ [((x, y), Empty) | x <- [2..(size-1)], y <- [1..size `div` 2-1]]
					++ [((size `div` 2, size `div` 2), Black), ((size `div` 2+1, size `div` 2+1), Black)]
					++ [((size `div` 2, size `div` 2+1), White), ((size `div` 2+1, size `div` 2), White)]

convertToPiece:: String -> Piece
convertToPiece "White" = White
convertToPiece "Black" = Black
convertToPiece "Empty" = Empty

-- find a piece at a certain position
findPieceWithPos:: Position -> ZBoard -> (Position, Piece)
findPieceWithPos (x, y) board = head [((x2, y2), p) | ((x2, y2), p) <- board, x == x2, y == y2]

-- sort the board
sortZBoard:: ZBoard -> ZBoard
sortZBoard board = [findPieceWithPos (x, y) board| x <- [1..borderSize board], y <- [1..borderSize board]]

-- return the size of the border
borderSize:: ZBoard -> Int
borderSize board = 4 -- (sqrt (length board))
					
-- check whether a move of a certain color is available					
checkAvailable:: Piece -> Position -> ZBoard -> Bool
checkAvailable White (x, y) board = foldr (||) False [next (x, y) (x2, y2) | ((x2, y2), Black) <- board]
checkAvailable Black (x, y) board = foldr (||) False [next (x, y) (x2, y2) | ((x2, y2), White) <- board]

-- check whether two positions are next to each other
next:: Position -> Position -> Bool
next (x1, y1) (x2, y2)
	| x1 == x2 && y1 == y2 + 1 = True
	| x1 == x2 && y1 == y2 - 1 = True
	| y1 == y2 && x1 == x2 + 1 = True
	| y1 == y2 && x1 == x2 - 1 = True
	| otherwise = False

-- find left border of the piece of the same color
findLeftBorder:: Piece -> Position -> ZBoard -> Int
findLeftBorder p (x, y) board = foldr min y [y2 | ((x2, y2), p2) <- board, x == x2, p == p2]

-- find right border of the piece of the same color
findRightBorder:: Piece -> Position -> ZBoard -> Int
findRightBorder p (x, y) board = foldr max y [y2 | ((x2, y2), p2) <- board, x == x2, p == p2]

-- find up border of the piece of the same color
findUpBorder:: Piece -> Position -> ZBoard -> Int
findUpBorder p (x, y) board = foldr min x [x2 | ((x2, y2), p2) <- board, y == y2, p == p2]

-- find bottom border of the piece of the same color
findBottomBorder:: Piece -> Position -> ZBoard -> Int
findBottomBorder p (x, y) board = foldr max x [x2 | ((x2, y2), p2) <- board, y == y2, p == p2]

-- update the board after one move
move:: Piece -> Position -> ZBoard -> ZBoard
move Black (x, y) board
	| checkAvailable Black (x, y) board == False = sortZBoard board
	| otherwise = sortZBoard ([((x2, y2), Empty) | ((x2, y2), Empty) <- board, (x2 /= x || y2 /= y)] ++ [(a, Black) | (a, Black) <- board] ++ [((x, y), Black)]
		++ [((x2, y2), Black) | ((x2, y2), White) <- board, x == x2, elem y2 [(findLeftBorder Black (x, y) board)..(findRightBorder Black (x, y) board)]]
		++ [((x2, y2), Black) | ((x2, y2), White) <- board, y == y2, elem x2 [(findUpBorder Black (x, y) board)..(findBottomBorder Black (x, y) board)]]
		++ [((x2, y2), White) | ((x2, y2), White) <- board, x == x2, elem y2 ([1..(findLeftBorder Black (x, y) board)]++[(findRightBorder Black (x, y) board)..4])]
		++ [((x2, y2), White) | ((x2, y2), White) <- board, y == y2, elem x2 ([1..(findUpBorder Black (x, y) board)]++[(findBottomBorder Black (x, y) board)..4])]
		++ [((x2, y2), White) | ((x2, y2), White) <- board, x2 /= x, y2 /= y])

move White (x, y) board
	| checkAvailable White (x, y) board == False = sortZBoard board
	| otherwise = sortZBoard ([((x2, y2), Empty) | ((x2, y2), Empty) <- board, (x2 /= x || y2 /= y)] ++ [(a, White) | (a, White) <- board] ++ [((x, y), White)]
		++ [((x2, y2), White) | ((x2, y2), Black) <- board, x == x2, elem y2 [(findLeftBorder White (x, y) board)..(findRightBorder White (x, y) board)]]
		++ [((x2, y2), White) | ((x2, y2), Black) <- board, y == y2, elem x2 [(findUpBorder White (x, y) board)..(findBottomBorder White (x, y) board)]]
		++ [((x2, y2), Black) | ((x2, y2), Black) <- board, x == x2, elem y2 ([1..(findLeftBorder White (x, y) board)]++[(findRightBorder White (x, y) board)..4])]
		++ [((x2, y2), Black) | ((x2, y2), Black) <- board, y == y2, elem x2 ([1..(findUpBorder White (x, y) board)]++[(findBottomBorder White (x, y) board)..4])]
		++ [((x2, y2), Black) | ((x2, y2), Black) <- board, x2 /= x, y2 /= y])

{-testS = move Black (4,3)(move White (3,4) (move Black (1,3) (initialZBoard 4)))-}

{-boardToString:: ZBoard -> String-}
{-boardToString board = concat [(show p) | ((x, y), p) <- board]-}

{-getInt :: IO Int-}
{-getInt = do-}
	{-line <- getLine-}
	{-return (read line :: Int)-}
	
{-startGame:: ZBoard -> IO()-}
{-startGame board = do-}
	{-line <- getLine-}
	{-x <- getInt-}
	{-y <- getInt-}
	
	{-putStrLn (boardToString (move (convertToPiece line) (x,y) board))-}
	{-startGame (move (convertToPiece line) (x,y) board)-}

