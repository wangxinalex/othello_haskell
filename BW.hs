module BW where
import System.Random
import System.Time
--module BW where
data Piece = White | Black | Empty deriving (Eq, Show)
type Position = (Int, Int)
type ZBoard = [(Position, Piece)]

type Board = [Piece]
type Step = (Position, Piece) -- ((Int, Int), Piece)

boardWidth :: Int
boardWidth = 8

getPosition :: Step->Position
getPosition ((x,y),piece) = (x,y)

getPiece :: Step->Piece
getPiece ((x,y),piece) = piece

index2Position :: Int -> Position
index2Position i = ((i `mod` boardWidth) , (i `div` boardWidth))

position2Index:: Position -> Int
position2Index (x, y) = (y * boardWidth + x)

{-generate the initial board-}
newBoard :: Int -> Board
newBoard width = (replicate pad Empty) ++ [White, Black] ++ (replicate (width - 2) Empty) ++ [Black, White] ++ (replicate pad Empty) where pad = (width+1)*((width `div` 2) - 1)

reinitializeBoard :: Board -> Board
reinitializeBoard b = newBoard boardWidth

reinitializeColor::Piece->Piece
reinitializeColor color = Black

{-check if the game ends-}
isGameEnd :: Board -> Piece -> Bool
isGameEnd board piece = ((countPieces board Empty) == 0) || ((length $ allValidPositions board piece) == 0)

{-return the color of the winner, Empty for a draw-}
whoWins ::  Board -> Piece -> Piece
whoWins  board piece
    | countPieces board piece > boardWidth * boardWidth `div` 2 = piece
    | countPieces board piece < boardWidth * boardWidth `div` 2 = getOppositeColor piece
    | countPieces board piece == boardWidth * boardWidth `div` 2 = Empty 

countPieces :: Board -> Piece -> Int
countPieces board color = length [piece | piece <- board, piece == color]

getOppositeColor::Piece->Piece
getOppositeColor piece
    | piece == Black = White
    | piece == White = Black
    | otherwise = error "No opposite color"

{-check whether this step locates in the boundary of the board-}
stepInBoard :: Board -> Step -> Bool
stepInBoard board ((x,y),position) = x >= 0 && x < boardWidth && y >= 0 && y < boardWidth 

{-check whether this step locates in am empty grid-}
stepInEmptyGrid :: Board -> Step -> Bool
stepInEmptyGrid board step = board !! (position2Index (getPosition step)) == Empty

{-check whether this step can reverse other pieces-}
stepCanReverse :: Board -> Step -> Bool
stepCanReverse board step 
    = foldr (||) False (map (besiegeOpposite board step) [p | p <- findPiecesSameColor board (getPiece step)])

{-check whether the step is valid.
 1. within the range of board
 2. no picec in the current position -}
validStep :: Board -> Step -> Bool
validStep board step = (stepInBoard board step) && (stepInEmptyGrid board step) && (stepNext board step) && (stepCanReverse board step)

allValidPositions :: Board -> Piece -> [Position]
allValidPositions board piece = [(x,y)| x <- [0 .. (boardWidth - 1)], y <- [0 .. (boardWidth - 1)], validStep board ((x,y), piece)]

{-put a new step on the board-}
putThisPiece :: Step -> Board -> Board
putThisPiece step board = (take index board) ++ (getPiece step) : (drop (index + 1) board) where index = position2Index (getPosition step)

{-reverse the pieces after this step-}
reversePieces :: Step -> Board -> Board
reversePieces step board = foldl (reversePiece (getPiece step)) board (positionReversed board step)

reversePiece :: Piece -> Board -> Position -> Board
reversePiece piece board position = (take index board) ++ piece : (drop (index+1) board) where index = position2Index position

{-find all pieces in the opposite color that can be reversed by this step-}
positionReversed :: Board -> Step -> [Position]
positionReversed board step
    =  concat [allPositionsInBetween (getPosition step) p |p <- positionInPair board step] 
    
{-sortPosition :: [Position] -> [Position]-}
{-sortPosition [] = []-}
{-sortPosition (p:ps)-}
    {-= [lp | lp <- ps, (position2Index lp) < (position2Index p)]++[p]++[ gp | gp <- ps, (position2Index gp) > (position2Index p)]-}

{-find all pieces in the same color so as to form a pair -}
positionInPair :: Board -> Step -> [Position]
positionInPair board step
    = [p | p <- findPiecesSameColor board (getPiece step), besiegeOpposite board step p]
    
{-check whether this step is adjacent to other pieces-}
stepNext :: Board -> Step -> Bool
stepNext board step  = foldr (||) False [nextPosition (getPosition step) step2| step2 <- findallPieces board ]

{-find all positions occupied by a piece-}
findallPieces :: Board -> [Position]
findallPieces [] = []
findallPieces board = findallPieces_ board 0

findallPieces_ :: Board -> Int -> [Position]
findallPieces_ [] i = []
findallPieces_ (b:bs) i
    | b == Empty  =  findallPieces_ bs (i+1)
    | otherwise   =  (index2Position i): findallPieces_ bs (i+1)

{-find all positions occupied by a piece of the same color-}
findPiecesSameColor :: Board -> Piece -> [Position]
findPiecesSameColor [] step = []
findPiecesSameColor board piece = findPiecesSameColor_ board piece 0

findPiecesSameColor_ :: Board -> Piece -> Int -> [Position]
findPiecesSameColor_ [] piece i = []
findPiecesSameColor_ (b:bs) piece i 
    | b == piece  = (index2Position i) : findPiecesSameColor_ bs piece (i+1)
    | otherwise   = findPiecesSameColor_ bs piece (i+1)

{-check whether two positions are adjacent to each other-}
nextPosition :: Position -> Position -> Bool
nextPosition (x1, y1) (x2, y2)  = 
     (((abs (x1 - x2)) == 1) && (abs (y1 - y2) <= 1)) || 
     (((abs (y1 - y2)) == 1) && (abs (x1 - x2) <= 1)) 
    
{-check whether this step can besiege opponent pieces with a certain position
  1. the position must be the same color 
  2. all positions between them must be occupied by the opponent pieces -}
besiegeOpposite :: Board -> Step -> Position -> Bool
besiegeOpposite board step p2  
    | not (sameColor piece (getPieceOnBoard board p2)) = False
    | not (positionInLine p1 p2) = False
    | (distancePosition p1 p2) < 2 = False
    | otherwise = foldr (&&) True (map (oppositeColor piece) (map (getPieceOnBoard board) [p | p <- allPositionsInBetween p1 p2])) 
        where p1    = getPosition step
              piece = getPiece step

{-check whether two pieces are of the opposite color-}
oppositeColor :: Piece -> Piece -> Bool
oppositeColor pi1 pi2 = (pi1 == White && pi2 == Black) || (pi1 == Black && pi2 == White)

{-check if two pieces are of the same color (both not empty)-}
sameColor :: Piece -> Piece -> Bool
sameColor pi1 pi2 = (pi1 == White && pi2 == White) || (pi1 == Black && pi2 == Black)

{-check whether two positions are in a line-}
positionInLine :: Position -> Position -> Bool
positionInLine (x1, y1) (x2, y2) 
    = (x1 == x2) || (y1 == y2) || abs(x1 - x2) == abs (y1 - y2)

{-find all position between two positions-}
allPositionsInBetween :: Position -> Position -> [Position]
allPositionsInBetween p1 p2
    | not (positionInLine p1 p2) = []
    | distancePosition p1 p2 < 2 = []
    | otherwise                  = allPositionsInBetween_ p1 p2

allPositionsInBetween_ :: Position -> Position -> [Position]
allPositionsInBetween_ (x1, y1) (x2, y2)
    | x1 == x2  = [(x1, y) | y <- [(y1' + 1) .. (y2' - 1)]] 
    | y1 == y2  = [(x, y1) | x <- [(x1' + 1) .. (x2' - 1)]]
    | otherwise = [(x, y)  | x <- [(x1' + 1) .. (x2' - 1)], y <- [(y1' + 1) .. (y2' - 1)], (abs (x - x1)) == (abs (y - y1))]
            where y1' = min y1 y2
                  y2' = max y1 y2
                  x1' = min x1 x2
                  x2' = max x1 x2

{-get the piece of a certain position on the board-}
getPieceOnBoard :: Board -> Position -> Piece
getPieceOnBoard board p = board !! (position2Index p)

{-find the number of positions between two positions-}
distancePosition :: Position -> Position -> Int
distancePosition (x1,y1) (x2,y2) 
    | x1 - x2 == 0 = abs (y1 - y2)
    | otherwise    = abs (x1 - x2)

generateRandom::Int->IO Int
generateRandom max
    = do r1 <- getStdGen
         let (x, r2) = randomR (0, max) r1
         setStdGen r2
         return x

-- find all possible steps
findAvailableSteps:: Piece -> Board -> [Step]
findAvailableSteps piece board = filter (validStep board) [((x, y), piece) | x <- [0.. (boardWidth - 1)], y <- [0.. (boardWidth - 1)]]

-- easy AI: randomly find an available step
easyAI:: Piece -> Board -> Step
easyAI piece board = (findAvailableSteps piece board) !! (getRan (length (findAvailableSteps piece board)) - 1)

-- get a random number of range
getRan:: Int -> Int
getRan range = getFrom (randomR (1, range) (mkStdGen 1))

getFrom:: (Int,StdGen) -> Int
getFrom (a, b) = a

getFrom2:: (Step,Int) -> Step
getFrom2 (a, b) = a

-- get the step that could reverse most chesses
mediumAI:: Piece -> Board -> Step
mediumAI piece board = getFrom2 $ last $ quick_sort (zip (findAvailableSteps piece board) (map (reverseNum board) (findAvailableSteps piece board)))

-- get the reverse number of a certain chess
reverseNum:: Board -> Step -> Int
reverseNum board s = length (positionReversed board s)

-- QuickSort
quick_sort:: [(Step, Int)] -> [(Step, Int)]
quick_sort [] = []
quick_sort ((x, i):[]) = [(x, i)]
quick_sort ((x, i): xs) =
	let smaller_or_equal_list = [(a, b)| (a, b)<-xs, b<=i]
	    larger_list = [(a, b)| (a, b)<-xs, b>i]
	in quick_sort smaller_or_equal_list ++ [(x, i)] ++ quick_sort larger_list

-- get the step that could reverse most chesses using dynamic programming
--hardAI:: Piece -> Board -> (Step, Int)
--hardAI piece board

hardStep:: Int -> Board -> Int -> Step -> Int
hardStep level board weight step
	| level == 0 = weight
	| otherwise = 
		let
			newBoard = putThisPiece step (reversePieces step board)) weight
		in
			weight + (hardStep (level-1) newBoard
	
newBoard2:: Board
newBoard2 = [Empty, Empty, Empty, Empty, Empty, White, Black, Empty, Empty, Black, White, Empty, Empty, Empty, Empty, Empty]
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- initial a board of size n with four pieces occupied
{-initialZBoard:: Int -> ZBoard-}
{-initialZBoard size = [((x, y), Empty) | x <- [1..size `div` 2-1], y <- [1..size]]-}
					{-++ [((x, y), Empty) | x <- [size `div` 2+2..size], y <- [1..size]]-}
					{-++ [((x, y), Empty) | x <- [2..(size-1)], y <- [size `div` 2+2..size]]-}
					{-++ [((x, y), Empty) | x <- [2..(size-1)], y <- [1..size `div` 2-1]]-}
					{-++ [((size `div` 2, size `div` 2), Black), ((size `div` 2+1, size `div` 2+1), Black)]-}
					{-++ [((size `div` 2, size `div` 2+1), White), ((size `div` 2+1, size `div` 2), White)]-}

{-convertToPiece:: String -> Piece-}
{-convertToPiece "White" = White-}
{-convertToPiece "Black" = Black-}
{-convertToPiece "Empty" = Empty-}

{--- find a piece at a certain position-}
{-findPieceWithPos:: Position -> ZBoard -> (Position, Piece)-}
{-findPieceWithPos (x, y) board = head [((x2, y2), p) | ((x2, y2), p) <- board, x == x2, y == y2]-}

{--- sort the board-}
{-sortZBoard:: ZBoard -> ZBoard-}
{-sortZBoard board = [findPieceWithPos (x, y) board| x <- [1..borderSize board], y <- [1..borderSize board]]-}

-- return the size of the border
{-borderSize:: ZBoard -> Int-}
{-borderSize board = 4 -- (sqrt (length board))-}
					
-- check whether a move of a certain color is available					
{-checkAvailable:: Piece -> Position -> ZBoard -> Bool-}
{-checkAvailable White (x, y) board = foldr (||) False [next (x, y) (x2, y2) | ((x2, y2), Black) <- board]-}
{-checkAvailable Black (x, y) board = foldr (||) False [next (x, y) (x2, y2) | ((x2, y2), White) <- board]-}

-- check whether two positions are next to each other
{-next:: Position -> Position -> Bool-}
{-next (x1, y1) (x2, y2)-}
	{-| x1 == x2 && y1 == y2 + 1 = True-}
	{-| x1 == x2 && y1 == y2 - 1 = True-}
	{-| y1 == y2 && x1 == x2 + 1 = True-}
	{-| y1 == y2 && x1 == x2 - 1 = True-}
	{-| otherwise = False-}

{--- find left border of the piece of the same color-}
{-findLeftBorder:: Piece -> Position -> ZBoard -> Int-}
{-findLeftBorder p (x, y) board = foldr min y [y2 | ((x2, y2), p2) <- board, x == x2, p == p2]-}

{--- find right border of the piece of the same color-}
{-findRightBorder:: Piece -> Position -> ZBoard -> Int-}
{-findRightBorder p (x, y) board = foldr max y [y2 | ((x2, y2), p2) <- board, x == x2, p == p2]-}

{--- find up border of the piece of the same color-}
{-findUpBorder:: Piece -> Position -> ZBoard -> Int-}
{-findUpBorder p (x, y) board = foldr min x [x2 | ((x2, y2), p2) <- board, y == y2, p == p2]-}

{--- find bottom border of the piece of the same color-}
{-findBottomBorder:: Piece -> Position -> ZBoard -> Int-}
{-findBottomBorder p (x, y) board = foldr max x [x2 | ((x2, y2), p2) <- board, y == y2, p == p2]-}

{--- update the board after one move-}
{-move:: Piece -> Position -> ZBoard -> ZBoard-}
{-move Black (x, y) board-}
	{-| checkAvailable Black (x, y) board == False = sortZBoard board-}
	{-| otherwise = sortZBoard ([((x2, y2), Empty) | ((x2, y2), Empty) <- board, (x2 /= x || y2 /= y)] ++ [(a, Black) | (a, Black) <- board] ++ [((x, y), Black)]-}
		{-++ [((x2, y2), Black) | ((x2, y2), White) <- board, x == x2, elem y2 [(findLeftBorder Black (x, y) board)..(findRightBorder Black (x, y) board)]]-}
		{-++ [((x2, y2), Black) | ((x2, y2), White) <- board, y == y2, elem x2 [(findUpBorder Black (x, y) board)..(findBottomBorder Black (x, y) board)]]-}
		{-++ [((x2, y2), White) | ((x2, y2), White) <- board, x == x2, elem y2 ([1..(findLeftBorder Black (x, y) board)]++[(findRightBorder Black (x, y) board)..4])]-}
		{-++ [((x2, y2), White) | ((x2, y2), White) <- board, y == y2, elem x2 ([1..(findUpBorder Black (x, y) board)]++[(findBottomBorder Black (x, y) board)..4])]-}
		{-++ [((x2, y2), White) | ((x2, y2), White) <- board, x2 /= x, y2 /= y])-}

{-move White (x, y) board-}
	{-| checkAvailable White (x, y) board == False = sortZBoard board-}
	{-| otherwise = sortZBoard ([((x2, y2), Empty) | ((x2, y2), Empty) <- board, (x2 /= x || y2 /= y)] ++ [(a, White) | (a, White) <- board] ++ [((x, y), White)]-}
		{-++ [((x2, y2), White) | ((x2, y2), Black) <- board, x == x2, elem y2 [(findLeftBorder White (x, y) board)..(findRightBorder White (x, y) board)]]-}
		{-++ [((x2, y2), White) | ((x2, y2), Black) <- board, y == y2, elem x2 [(findUpBorder White (x, y) board)..(findBottomBorder White (x, y) board)]]-}
		{-++ [((x2, y2), Black) | ((x2, y2), Black) <- board, x == x2, elem y2 ([1..(findLeftBorder White (x, y) board)]++[(findRightBorder White (x, y) board)..4])]-}
		{-++ [((x2, y2), Black) | ((x2, y2), Black) <- board, y == y2, elem x2 ([1..(findUpBorder White (x, y) board)]++[(findBottomBorder White (x, y) board)..4])]-}
		{-++ [((x2, y2), Black) | ((x2, y2), Black) <- board, x2 /= x, y2 /= y])-}

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

