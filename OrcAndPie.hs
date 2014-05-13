import qualified Data.Map.Lazy as Map
import System.Console.ANSI

main = do
    putStrLn "This is a test"
    putStrLn "This text should be overwritten."
    let gameMap = mapWithRoom (35,7) (44,16)
    drawMap gameMap

drawMap gameMap = do
    mapM_ drawTile $ viewMap gameMap
    setCursorPosition 25 0

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
mapWithRoom topLeft bottomRight = Map.empty

terrainAt :: Position -> GameMap -> Terrain
terrainAt = Map.findWithDefault NoTerrain
