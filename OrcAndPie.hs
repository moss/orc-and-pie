import Control.Monad
import System.Random

import AnsiRendering
import Game
import Roguelike

main :: IO ()
main = do
    initScreen
    input <- getContents
    orcBrain <- getStdGen
    let gameStates = play newGame $ gameMoves input orcBrain
    forM_ gameStates render
    restoreSettings
