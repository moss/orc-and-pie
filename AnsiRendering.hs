module AnsiRendering where

import System.Console.ANSI

import Renderable

render gameState = do
    mapM_ drawTile $ viewTiles gameState
    setCursorPosition 25 0

drawTile (x,y,char) = do
    setCursorPosition y x
    putChar char
