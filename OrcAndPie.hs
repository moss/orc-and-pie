import AnsiRendering
import Game
import Roguelike

main :: IO ()
main = do
    render newGame
    input <- getChar
    render $ advance newGame input
