module Game where

import qualified Data.Map.Lazy as Map

import Renderable

data GameState = Playing { pMap :: GameMap, pPlayerLocation :: Position }
type GameMap = Map.Map Position Terrain
data Terrain = NoTerrain | Floor

newGame = Playing { pMap = mapWithRoom (35,7) (44,16)
                  , pPlayerLocation = (40, 11)
                  }

mapWithRoom :: Position -> Position -> GameMap
mapWithRoom topLeft bottomRight = Map.fromList
  [(pos, Floor) | pos <- positionsInRange topLeft bottomRight]

positionsInRange :: Position -> Position -> [Position]
positionsInRange (left,top) (right,bottom) = [(x,y) | x <- [left..right], y <- [top..bottom]]

terrainAt :: Position -> GameMap -> Terrain
terrainAt = Map.findWithDefault NoTerrain

instance Renderable GameState where
    viewTile position gameState | pPlayerLocation gameState == position = '@'
    viewTile position Playing { pMap = gameMap } = viewTerrain position gameMap

viewTerrain position gameMap = case terrainAt position gameMap of
                                 Floor -> '.'
                                 NoTerrain -> ' '
