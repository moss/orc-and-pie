import Control.Monad

import AnsiRendering
import Game
import Roguelike

main :: IO ()
main = do
    initScreen
    input <- getContents
    let gameStates = play newGame $ gameMoves input
    forM_ gameStates render
    restoreSettings
