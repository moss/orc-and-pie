module AnsiRendering where

import System.Console.ANSI
import System.IO

import Roguelike

initScreen = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    hideCursor
    clearScreen

restoreSettings = do
    showCursor

render gameState = do
    setCursorPosition 0 0
    clearLine
    putStr $ viewMessage gameState
    mapM_ drawTile $ viewTiles gameState
    setCursorPosition 25 0

drawTile (x,y,char) = do
    setCursorPosition y x
    putChar char
