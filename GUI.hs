{-module GUI where-}
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.Maybe

levels :: [(String, Int)]
levels = [("Easy", 2), ("Medium", 4), ("Hard", 8)]

boardIndex = 0
blackIndex = 1
whiteIndex = 2

boardWidth :: Int
boardWidth = 8

defaultLevel :: Int
defaultLevel = 2

padding :: Int
padding = 27

width :: Int
width = 56

type Board = [Position]
data Position = Black | White | Empty deriving Eq
type Step = (Int, Int, Position)

newBoard :: Int -> Board
newBoard width = replicate (width*width) Empty

getPosition::(Int, Int)->(Int,Int)
getPosition (x,y) = 
          let x_pos = ((x - padding) `div` width) 
              y_pos = ((y - padding) `div` width)
          in (x_pos, y_pos) 

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
                                   help := "About the game rules"]
        about <- menuAbout hlpMenu [help := "About the program"]

        set boardPanel [on resize := repaint boardPanel,
                        on paint  := drawBackground [boardBmp, blackPieceBmp, whitePieceBmp] gameBoard,
                        on click  := putPieces boardPanel gameBoard currentColor]

        set f [statusBar := [status],
               menuBar   := [gameMenu, optMenu, hlpMenu],
               layout    :=   minsize (sz 500 500) $ widget boardPanel]

putPieces :: Panel() -> Var Board -> Var Position -> Point -> IO ()
putPieces pan varBoard varColor (Point x y) 
    = do 
         color <- varGet varColor
         let (x_pos, y_pos) = getPosition (x,y)
             step = (x_pos, y_pos, color)
         board <- varGet varBoard
         putStrLn $ "x = " ++ show x_pos ++ ", y = "++ show y_pos
         varUpdate varBoard (changeBoard step)
         if x_pos == 0
         then putStrLn "0"
         varUpdate varColor (changeColor)
         repaint pan
         return ()
        
changeColor :: Position -> Position
changeColor color
         | color == Black  = White
         | color == White  = Black

changeBoard :: Step -> Board -> Board
changeBoard (x,y,position) board 
         | x < 0 || y < 0 || x >= boardWidth || y >= boardWidth = board          
         | otherwise = (take index board) ++ position : (drop (index + 1) board) where index = positionToIndex x y  

positionToIndex:: Int -> Int -> Int
positionToIndex x y = (y * boardWidth + x)

drawBackground :: [Bitmap()] -> Var Board -> DC() -> Rect -> IO()
drawBackground bmps varPieces dc (Rect x y w h) = 
        do pieces <- varGet varPieces
           drawBitmap dc (bmps !! boardIndex) pointZero False []
           drawPieces dc pieces bmps
           return ()

drawPieces :: DC() -> Board ->[Bitmap()] -> IO ()
drawPieces dc gameBoard bmps = 
    do 
        for 0 ((length gameBoard) - 1) (\i ->
               case gameBoard !! i of
                    Black -> drawBitmap dc (bmps !! blackIndex) (generatePosition i) False []
                    White -> drawBitmap dc (bmps !! whiteIndex) (generatePosition i) False []
                    _     -> return () )
        return ()

for :: Int -> Int -> (Int -> IO ()) -> IO ()
for x y f = sequence_ $ map f [x..y]

generatePosition :: Int -> Point
generatePosition index  
            | index < 0 || index >= boardWidth*boardWidth = error "Invalid position"
            | otherwise = pt x y 
                            where x = (index `mod` boardWidth)*width+padding
                                  y = (index `div` boardWidth)*width+padding
