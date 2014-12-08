{-module GUI where-}
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.Maybe

levels :: [(String, Int)]
levels = [("Easy", 2), ("Medium", 4), ("Hard", 8)]

defaultLevel :: Int
defaultLevel = 2

main = start gui

gui :: IO ()
gui = do
		f <- frame [text := "Othello" , picture := "./img/icon.png"]
		
		boardBmp <- bitmapCreateLoad "./img/board.png" wxBITMAP_TYPE_PNG

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
						on paint  := drawBackground boardBmp]

		set f [statusBar := [status],
			   menuBar   := [gameMenu, optMenu, hlpMenu],
			   layout    :=   minsize (sz 500 500) $ widget boardPanel]

drawBackground :: Bitmap() -> DC() -> Rect -> IO()
drawBackground bmp dc (Rect x y w h) = 
		do 
			drawBitmap dc bmp pointZero False []
			return ()



