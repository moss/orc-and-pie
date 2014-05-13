module Game where

import qualified Data.Map.Lazy as Map

import Renderable

data GameState = GameState { gsMap :: GameMap, gsPlayer :: Position }
type GameMap = Map.Map Position Terrain
data Terrain = NoTerrain | Floor

newGame = GameState { gsMap = mapWithRoom (35,7) (44,16)
                    , gsPlayer = (40, 11)
                    }

mapWithRoom :: Position -> Position -> GameMap
mapWithRoom topLeft bottomRight = Map.fromList
  [(pos, Floor) | pos <- positionsInRange topLeft bottomRight]

positionsInRange :: Position -> Position -> [Position]
positionsInRange (left,top) (right,bottom) = [(x,y) | x <- [left..right], y <- [top..bottom]]

terrainAt :: Position -> GameMap -> Terrain
terrainAt = Map.findWithDefault NoTerrain

instance Renderable GameState where
    viewTile position gameState | gsPlayer gameState == position = '@'
    viewTile position GameState { gsMap = gameMap } = viewTerrain position gameMap

viewTerrain position gameMap = case terrainAt position gameMap of
                                 Floor -> '.'
                                 NoTerrain -> ' '
