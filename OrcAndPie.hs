import AnsiRendering
import Game
import Roguelike
import Control.Monad

main :: IO ()
main = do
    input <- getContents
    let gameStates = scanl advance newGame input 
    forM_ gameStates render
