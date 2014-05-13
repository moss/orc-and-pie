import System.Console.ANSI

import Renderable
import Game

main = do
    let gameState = Playing (mapWithRoom (35,7) (44,16))
    render gameState

-- Rendering

render gameState = do
    mapM_ drawTile $ viewTiles gameState
    setCursorPosition 25 0

drawTile (x,y,char) = do
    setCursorPosition y x
    putChar char
