import AnsiRendering
import Game

main = do
    let gameState = Playing (mapWithRoom (35,7) (44,16))
    render gameState
