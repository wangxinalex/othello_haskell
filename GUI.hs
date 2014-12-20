{-module GUI where-}
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.Maybe
import BW

levels :: [(String, Int)]
levels = [("Easy", 2), ("Medium", 4), ("Hard", 8)]

boardIndex = 0
blackIndex = 1
whiteIndex = 2

defaultLevel :: Int
defaultLevel = 2

padding :: Int
padding = 27

width :: Int
width = 56

boardWidth :: Int
boardWidth = 8

newBoard :: Int -> Board
newBoard width = (replicate pad Empty) ++ [White, Black] ++ (replicate (width - 2) Empty) ++ [Black, White] ++ (replicate pad Empty) where pad = (width+1)*((width `div` 2) - 1)

index2Position :: Int -> Position
index2Position i = ((i `mod` boardWidth) , (i `div` boardWidth))

board2ZBoard :: Board -> ZBoard
board2ZBoard board
    = b2Z 0 board

b2Z :: Int -> Board -> ZBoard
b2Z i [] = []
b2Z i (b:bs) = ((index2Position i), b) : b2Z (i+1) bs

zBoard2Board :: ZBoard -> Board
zBoard2Board zb = zBoard2Board_ (sortZBoard zb)

zBoard2Board_ :: ZBoard -> Board
zBoard2Board_ [] = []
zBoard2Board_ (((x,y),piece):zbs) = piece : zBoard2Board zbs

main = start gui

gui :: IO ()
gui = do
        gameBoard <- varCreate (newBoard boardWidth)
        currentColor <- varCreate (Black)
        f <- frame [text := "Othello" , picture := "./img/icon.png"]
        
        boardBmp <- bitmapCreateLoad "./img/board.png" wxBITMAP_TYPE_PNG
        whitePieceBmp <- bitmapCreateLoad "./img/white.png" wxBITMAP_TYPE_PNG
        blackPieceBmp <- bitmapCreateLoad "./img/black.png" wxBITMAP_TYPE_PNG

        boardPanel <- panel f [clientSize := sz 500 500 ] 
        
        status <- statusField [text := "Welcome to Othello"]
        gameMenu <- menuPane [text := "&Game"]
        new <- menuItem gameMenu [text := "New",
                                  help := "Restart the game"]
        menuLine gameMenu
        menuQuit gameMenu [help := "Quit the game",
                           on command := close f]

        optMenu <- menuPane [text := "Options"]
        r0 <- menuItem optMenu [text := "Human first",
                                help := "Choose starting player",
                                checkable := True, checked := True]
        menuLine optMenu
        rs <- sequence [menuRadioItem optMenu 
                        [text := txt, 
                         help := "Choose the opponent level"]
                        |(txt, l) <- levels]
        sequence_ [set r [checked := True]
                    | (r,l) <- zip rs (map snd levels), 
                        l == defaultLevel]

        hlpMenu <- menuHelp []
        rules <- menuItem hlpMenu [text := "Rules",
                                   help := "About the game rules",
                                   on command := infoRules f]

        about <- menuAbout hlpMenu [help := "About the program",
                                    on command := infoAbout f]

        set boardPanel [on resize := repaint boardPanel,
                        on paint  := drawBackground [boardBmp, blackPieceBmp, whitePieceBmp] gameBoard,
                        on click  := putPieces boardPanel gameBoard currentColor]

        set f [statusBar := [status],
               menuBar   := [gameMenu, optMenu, hlpMenu],
               layout    :=  minsize (sz 500 500) $ widget boardPanel]

        set new [on command := do varUpdate gameBoard reinitializeBoard
                                  repaint boardPanel]

reinitializeBoard :: Board -> Board
reinitializeBoard b = newBoard boardWidth

infoAbout :: Frame a -> IO()
infoAbout w
     = infoDialog w "About Othello" $
       init $
       unlines ["Written in Haskell using the wxWidgets toolkit",
                "by wangxinalex <wangxinalex@gmail.com>",
                "and zccshome <zijinhuakaile@sina.com>",
                "Published by http://io.wxa.me"
                ]

infoRules :: Frame a -> IO()
infoRules w
     = infoDialog w "Rules of Othello" $
       init $
       unlines ["Player and Computer take turns putting pieces on the board.",
                "All consecutive pieces between two opposite pieces will be reversed.",
                "The first player who cannot put valid pieces loses the game."
                ]

putPieces :: Panel() -> Var Board -> Var Piece -> Point -> IO ()
putPieces pan varBoard varColor (Point x y) = 
    do color <- varGet varColor
       let (x_pos, y_pos) = getPosition (x,y)
           step = ((x_pos, y_pos), color)
       board <- varGet varBoard
       putStrLn $ "x = " ++ show x_pos ++ ", y = "++ show y_pos
       print (findPiecesSameColor board color)
       print (map (besiegeOpposite board step) (findPiecesSameColor board color))
       {-print (map (oppositeColor color) (map (getPiece board) (allPositionsInBetween (3,4) (x_pos, y_pos))))-}
       varUpdate varBoard (changeBoard step)
       newBoard <- varGet varBoard
       print (findallPieces newBoard)
       varUpdate varColor (changeColor board step)
       repaint pan
       return ()
        
{-check whether the step is valid.
 1. within the range of board
 2. no picec in the current position-}
validStep :: Board -> Step -> Bool
validStep board step = (stepInBoard board step) && (stepInEmptyGrid board step) && (stepNext board step) && (stepCanReverse board step)
 
stepInBoard :: Board -> Step -> Bool
stepInBoard board ((x,y),position) = x >= 0 && x < boardWidth && y >= 0 && y < boardWidth 

stepInEmptyGrid :: Board -> Step -> Bool
stepInEmptyGrid board ((x,y), position) = board !! (position2Index (x, y)) == Empty

stepCanReverse :: Board -> Step -> Bool
stepCanReverse board ((x,y), piece) 
    = foldr (||) False (map (besiegeOpposite board ((x,y),piece)) [p | p <- findPiecesSameColor board piece])

stepNext :: Board -> Step -> Bool
stepNext board ((x,y),p)  = foldr (||) False [nextPosition (x,y) step2| step2 <- findallPieces board ]


findallPieces :: Board -> [Position]
findallPieces [] = []
findallPieces board = findallPieces_ board 0

findallPieces_ :: Board -> Int -> [Position]
findallPieces_ [] i = []
findallPieces_ (b:bs) i
    | b == Empty  =  findallPieces_ bs (i+1)
    | otherwise   =  (index2Position i): findallPieces_ bs (i+1)

findPiecesSameColor :: Board -> Piece -> [Position]
findPiecesSameColor [] step = []
findPiecesSameColor board piece = findPiecesSameColor_ board piece 0

findPiecesSameColor_ :: Board -> Piece -> Int -> [Position]
findPiecesSameColor_ [] piece i = []
findPiecesSameColor_ (b:bs) piece i 
    | b == piece  = (index2Position i) : findPiecesSameColor_ bs piece (i+1)
    | otherwise   = findPiecesSameColor_ bs piece (i+1)

    
nextPosition :: Position -> Position -> Bool
nextPosition (x1, y1) (x2, y2)  = 
     (((abs (x1 - x2)) == 1) && (abs (y1 - y2) <= 1)) || 
     (((abs (y1 - y2)) == 1) && (abs (x1 - x2) <= 1)) 
    
besiegeOpposite :: Board -> Step -> Position -> Bool
besiegeOpposite board (p1, piece) p2  
    | not (sameColor piece (getPiece board p2)) = False
    | not (positionInLine p1 p2) = False
    | (distancePosition p1 p2) < 2 = False
    | otherwise = foldr (&&) True (map (oppositeColor piece) (map (getPiece board) [p | p <- allPositionsInBetween p1 p2])) 

oppositeColor :: Piece -> Piece -> Bool
oppositeColor pi1 pi2 = (pi1 == White && pi2 == Black) || (pi1 == Black && pi2 == White)

sameColor :: Piece -> Piece -> Bool
sameColor pi1 pi2 = (pi1 == White && pi2 == White) || (pi1 == Black && pi2 == Black)

positionInLine :: Position -> Position -> Bool
positionInLine (x1, y1) (x2, y2) 
    = (x1 == x2) || (y1 == y2) || abs(x1 - x2) == abs (y1 - y2)

allPositionsInBetween :: Position -> Position -> [Position]
allPositionsInBetween p1 p2
    | not (positionInLine p1 p2)                = []
    | distancePosition p1 p2 < 2                = []
    | otherwise = allPositionsInBetween_ p1 p2

allPositionsInBetween_ :: Position -> Position -> [Position]
allPositionsInBetween_ (x1, y1) (x2, y2)
    | x1 == x2  = [(x1, y) | y <- [(y1' + 1) .. (y2' - 1)]] 
    | y1 == y2  = [(x, y1) | x <- [(x1' + 1) .. (x2' - 1)]]
    | otherwise = [(x, y)  | x <- [(x1' + 1) .. (x2' - 1)], y <- [(y1' + 1) .. (y2' - 1)], (x - x1') == (y - y1')]
            where y1' = min y1 y2
                  y2' = max y1 y2
                  x1' = min x1 x2
                  x2' = max x1 x2

getPiece :: Board -> Position -> Piece
getPiece board p = board !! (position2Index p)

distancePosition :: Position -> Position -> Int
distancePosition (x1,y1) (x2,y2) 
    | x1 - x2 == 0 = abs (y1 - y2)
    | otherwise    = abs (x1 - x2)

{-stepCanReverse :: Board -> Step -> Bool-}
{-stepCanReverse = -}

{-change the piece color for a valid step-}
changeColor :: Board -> Step -> Piece -> Piece
changeColor board step color
    | not (validStep board step) = color       
    | color == Black  = White
    | color == White  = Black

{-change the board status for a valid step-}
changeBoard :: Step -> Board -> Board
changeBoard ((x,y),piece) board 
    | not (validStep board ((x,y),piece)) = board
    | otherwise = (take index board) ++ piece : (drop (index + 1) board) where index = position2Index (x, y)

position2ZPosition :: Position -> Position
position2ZPosition (x, y) = (x + 1, y + 1)

position2Index:: Position -> Int
position2Index (x, y) = (y * boardWidth + x)

drawBackground :: [Bitmap()] -> Var Board -> DC() -> Rect -> IO()
drawBackground bmps varPieces dc (Rect x y w h) = 
    do pieces <- varGet varPieces
       drawBitmap dc (bmps !! boardIndex) pointZero False []
       drawPieces dc pieces bmps
       return ()

drawPieces :: DC() -> Board ->[Bitmap()] -> IO ()
drawPieces dc gameBoard bmps = 
    do  for 0 ((length gameBoard) - 1) (\i ->
               case gameBoard !! i of
                    Black -> drawBitmap dc (bmps !! blackIndex) (generatePiece i) False []
                    White -> drawBitmap dc (bmps !! whiteIndex) (generatePiece i) False []
                    _     -> return () )
        return ()

for :: Int -> Int -> (Int -> IO ()) -> IO ()
for x y f = sequence_ $ map f [x..y]

getPosition::(Int, Int)->(Int,Int)
getPosition (x,y) = 
    let x_pos = ((x - padding) `div` width) 
        y_pos = ((y - padding) `div` width)
    in (x_pos, y_pos) 

generatePiece :: Int -> Point
generatePiece index  
    | index < 0 || index >= boardWidth*boardWidth = error "Invalid position"
    | otherwise = pt x y 
                  where x = (index `mod` boardWidth)*width+padding
                        y = (index `div` boardWidth)*width+padding
