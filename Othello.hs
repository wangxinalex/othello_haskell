{-module GUI where-}
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.Maybe
import BW

levels :: [(String, Level)]
levels = [("Easy", Easy), ("Medium", Medium), ("Hard", Hard), ("Hell", Hell)]

boardIndex = 0
blackIndex = 1
whiteIndex = 2

defaultLevel :: Level
defaultLevel = Easy

padding :: Int
padding = 27

width :: Int
width = 56

main = start gui

gui :: IO ()
gui = do
        gameBoard <- varCreate (newBoard boardWidth)
        currentColor <- varCreate (Black)
        varLevel <- varCreate (Easy)
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
                         help := "Choose the opponent level",
                         on command := setLevel varLevel l]
                        |(txt, l)  <- levels]
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
                        on click  := putPieces f boardPanel gameBoard currentColor varLevel]

        set f [statusBar := [status],
               menuBar   := [gameMenu, optMenu, hlpMenu],
               layout    :=  minsize (sz 500 500) $ widget boardPanel]

        set new [on command := do humanFirst <- get r0 checked
                                  varSet gameBoard (reinitializeBoard humanFirst)
                                  varSet currentColor (reinitializeColor humanFirst)
                                  repaint boardPanel]

setLevel:: Var Level -> Level -> IO() 
setLevel varLevel level 
    = do varSet varLevel level
         return ()

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

putPieces :: Frame a -> Panel() -> Var Board -> Var Piece -> Var Level-> Point -> IO ()
putPieces frame pan varBoard varColor varLevel (Point x y) = 
    do color <- varGet varColor
       board <- varGet varBoard
       let (x_pos, y_pos) = generatePosition (x,y)
           step = ((x_pos, y_pos), color)
       putStrLn $ "x = " ++ show x_pos ++ ", y = "++ show y_pos
       if (isGameEnd board color) then
           checkWin frame varBoard varColor
           else 
                if (noPositionToPut board color) then
                    do varUpdate varColor changeColor
                       computerPlay varBoard varColor varLevel
                       checkWin frame varBoard varColor
                       else 
                            if (not $ validStep board step) then
                                return ()
                                else
                                    do varUpdate varBoard (changeBoard step)
                                       varUpdate varColor changeColor
                                       color <- varGet varColor
                                       board <- varGet varBoard
                                       if (isGameEnd board color) then
                                           checkWin frame varBoard varColor 
                                           else 
                                                do computerPlay varBoard varColor varLevel
                                                   checkWin frame varBoard varColor
       repaint pan
       return ()

{-computer puts a piece-}
computerPlay :: Var Board -> Var Piece -> Var Level-> IO()
computerPlay varBoard varColor varLevel
    = do board <- varGet varBoard
         color <- varGet varColor
         if (noPositionToPut board color) then
             do varUpdate varColor changeColor
                return ()
                else 
                    do level <- varGet varLevel
                       case level of
                           Easy -> varUpdate varBoard (changeBoard (easyAI color board))
                           Medium -> varUpdate varBoard (changeBoard (mediumAI color board))
                           Hard -> varUpdate varBoard (changeBoard (hardAI color board))
                           Hell -> varUpdate varBoard (changeBoard (extremeAI color board))
                       varUpdate varColor changeColor
                       return ()
    
{-check if the game ends and display the winning message-}
checkWin::Frame a -> Var Board -> Var Piece -> IO()
checkWin frame varBoard varPiece
    = do board <- varGet varBoard
         piece <- varGet varPiece
         if (isGameEnd board piece) then
            do varSet varPiece (reinitializeColor True)
               varSet varBoard (reinitializeBoard True)
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
    = putThisPiece step (reversePieces step board) -- first reverse the pieces then put this new step on the board

{-change the piece color for a valid step-}
changeColor :: Piece -> Piece
changeColor color
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
                  where x = (index `mod` boardWidth) * width + padding
                        y = (index `div` boardWidth) * width + padding
