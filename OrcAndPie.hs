import qualified Data.Map.Lazy as Map
import System.Console.ANSI

main = do
    let gameMap = mapWithRoom (35,7) (44,16)
    drawMap gameMap

drawMap gameMap = do
    mapM_ drawTile $ viewMap gameMap
    setCursorPosition 25 0

addNumbers a b = a + b

addN

drawTile (x,y,char) = do
    setCursorPosition y x
    putChar char

columns = [0..79]
rows = [0..24]

viewMap :: GameMap -> [DisplayTile]
viewMap gameMap = [(x, y, viewTile (x,y) gameMap) | x <- columns, y <- rows]

viewTile (x,y) gameMap = case terrainAt (x,y) gameMap of
                           Floor -> '.'
                           NoTerrain -> ' '

type DisplayTile = (Int,Int,Char)

-- pure functions only below this point

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
