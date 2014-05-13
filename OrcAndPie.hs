import qualified Data.Map.Lazy as Map
import System.Console.ANSI

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

columns = [0..79]
rows = [0..24]

viewTiles :: GameState -> [DisplayTile]
viewTiles renderable = [(x, y, viewTile (x,y) renderable) | x <- columns, y <- rows]

viewTile :: Position -> GameState -> Char
viewTile (x,y) (Playing gameMap) = case terrainAt (x,y) gameMap of
                                     Floor -> '.'
                                     NoTerrain -> ' '

type DisplayTile = (Int,Int,Char)

-- Game Maps

data GameState = Playing (GameMap)
type GameMap = Map.Map Position Terrain
type Position = (Int,Int)
data Terrain = NoTerrain | Floor

mapWithRoom :: Position -> Position -> GameMap
mapWithRoom topLeft bottomRight = Map.fromList
  [(pos, Floor) | pos <- positionsInRange topLeft bottomRight]

positionsInRange :: Position -> Position -> [Position]
positionsInRange (left,top) (right,bottom) = [(x,y) | x <- [left..right], y <- [top..bottom]]

terrainAt :: Position -> GameMap -> Terrain
terrainAt = Map.findWithDefault NoTerrain
