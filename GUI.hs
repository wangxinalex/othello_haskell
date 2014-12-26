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
                        on click  := putPieces f boardPanel gameBoard currentColor]

        set f [statusBar := [status],
               menuBar   := [gameMenu, optMenu, hlpMenu],
               layout    :=  minsize (sz 500 500) $ widget boardPanel]

        set new [on command := do varUpdate gameBoard reinitializeBoard
                                  repaint boardPanel]

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

putPieces :: Frame a -> Panel() -> Var Board -> Var Piece -> Point -> IO ()
putPieces frame pan varBoard varColor (Point x y) = 
    do color <- varGet varColor
       let (x_pos, y_pos) = generatePosition (x,y)
           step = ((x_pos, y_pos), color)
       board <- varGet varBoard
       putStrLn $ "x = " ++ show x_pos ++ ", y = "++ show y_pos
       {-print (findPiecesSameColor board color)-}
       {-print (map (besiegeOpposite board step) (findPiecesSameColor board color))-}
       {-print $ positionReversed board step-}
       {-print (map (oppositeColor color) (map (getPiece board) (allPositionsInBetween (3,4) (x_pos, y_pos))))-}
       varUpdate varBoard (changeBoard step)
       {-newBoard <- varGet varBoard-}
       {-print (findallPieces newBoard)-}
       varUpdate varColor (changeColor board step)
       color <- varGet varColor
       board <- varGet varBoard
       print $ allValidPositions board color
       checkWin frame varBoard varColor 
       repaint pan
       return ()

{-check if the game ends and display the winning message-}
checkWin::Frame a -> Var Board -> Var Piece -> IO()
checkWin frame varBoard varPiece
    = do board <- varGet varBoard
         piece <- varGet varPiece
         if (isGameEnd board piece) then
            do 
               varUpdate varPiece reinitializeColor
               varUpdate varBoard reinitializeBoard
               infoWin frame (whoWins board piece)
            else 
               return ()

infoWin :: Frame a ->Piece-> IO()
infoWin frame piece
    = if piece == Empty then
        infoDialog frame "Game ends" $ init "Game ends in a draw."
        else 
            infoDialog frame "Game ends" $ init "Player " ++ (show piece) ++ " wins."

{-change the board status for a valid step-}
changeBoard :: Step -> Board -> Board
changeBoard step board 
    | not (validStep board step) = board
    | otherwise = putThisPiece step (reversePieces step board) -- first reverse the pieces then put this new step on the board

{-change the piece color for a valid step-}
changeColor :: Board -> Step -> Piece -> Piece
changeColor board step color
    | not (validStep board step) = color       
    | color == Black  = White
    | color == White  = Black

drawBackground :: [Bitmap()] -> Var Board -> DC() -> Rect -> IO()
drawBackground bmps varPieces dc (Rect x y w h) = 
    do pieces <- varGet varPieces
       drawBitmap dc (bmps !! boardIndex) pointZero False []
       drawPieces dc pieces bmps
       return ()

for :: Int -> Int -> (Int -> IO ()) -> IO ()
for x y f = sequence_ $ map f [x..y]

drawPieces :: DC() -> Board ->[Bitmap()] -> IO ()
drawPieces dc gameBoard bmps = 
    do  for 0 ((length gameBoard) - 1) (\i ->
               case gameBoard !! i of
                    Black -> drawBitmap dc (bmps !! blackIndex) (generatePiece i) False []
                    White -> drawBitmap dc (bmps !! whiteIndex) (generatePiece i) False []
                    _     -> return () )
        return ()

{-translate the position of the screen to the position on the board-}
generatePosition::(Int, Int)->(Int,Int)
generatePosition (x,y) = 
    let x_pos = ((x - padding) `div` width) 
        y_pos = ((y - padding) `div` width)
    in (x_pos, y_pos) 

generatePiece :: Int -> Point
generatePiece index  
    | index < 0 || index >= boardWidth*boardWidth = error "Invalid position"
    | otherwise = pt x y 
                  where x = (index `mod` boardWidth)*width+padding
                        y = (index `div` boardWidth)*width+padding
