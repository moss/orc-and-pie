import Control.Monad

import AnsiRendering
import Game
import Roguelike

main :: IO ()
main = do
    initScreen
    input <- getContents
    let gameStates = scanl advance newGame input
    let gameUntilEnd = takeWhile notOver gameStates
    forM_ gameUntilEnd render
    restoreSettings
