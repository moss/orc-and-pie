module GameState where

import GameMap
import Roguelike
import TerrainView

data GameState = GameState { gsMap :: GameMap
                           , gsPlayer :: Position
                           , gsOrc :: Position
                           , gsMessage :: String
                           }
               | QuitGame
type PositionGetter = GameState -> Position
type PositionSetter = GameState -> Position -> GameState

newGame = GameState { gsMap = mapWithRoom (35,7) (44,16)
                    , gsPlayer = (42, 9)
                    , gsOrc = (37, 14)
                    , gsMessage = "You see a horrible orc!"
                    }

instance Roguelike GameState where
    isOver QuitGame = True
    isOver gameState = False
    viewMessage = gsMessage
    viewTile position gameState | gsPlayer gameState == position = '@'
    viewTile position gameState | gsOrc gameState == position = 'o'
    viewTile position GameState { gsMap = gameMap } = viewTerrain position gameMap
    viewTiles gameState = [(x, y, viewTile (x,y) gameState) | (x,y) <- positionsOnMap $ gsMap gameState]

setPlayer gameState player = gameState { gsPlayer = player, gsMessage = "" }
setOrc gameState orc = gameState { gsOrc = orc, gsMessage = "" }

walkableTerrain position gameMap = terrainAt position gameMap == Floor
unoccupied position gameState = (gsOrc gameState) /= position
                              && (gsPlayer gameState) /= position
