module AnsiRendering
( initScreen, render, restoreSettings )
where

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
    hSetEcho stdin True
    putStrLn ""

render gameState = do
    setCursorPosition 0 0
    clearLine
    putStr $ viewMessage gameState
    mapM_ drawTile $ viewTiles gameState
    setCursorPosition 25 0
    putStr $ viewStatus gameState 

drawTile (x,y,char) = do
    setCursorPosition y x
    putChar char
