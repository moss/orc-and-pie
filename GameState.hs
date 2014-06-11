module GameState where

import GameMap
import Roguelike
import TerrainView

data GameState = GameState { gsMap :: GameMap
                           , gsPlayer :: Character
                           , gsOrc :: Character
                           , gsMessage :: GameMessage
                           }
               | QuitGame deriving (Show)
data GameMessage = NoMessage | SeeOrc | OrcHits deriving (Eq, Show)
data Character = Character { cHitPoints :: Int
                           , cPosition :: Position
                           } deriving (Show, Eq)
type CharacterGetter = GameState -> Character
type CharacterSetter = Character -> GameState -> GameState

newGame = GameState { gsMap = mapWithRoom (35,7) (44,16)
                    , gsPlayer = Character 100 (42, 9)
                    , gsOrc = Character 100 (37, 14)
                    , gsMessage = SeeOrc
                    }

instance Roguelike GameState where
    isOver QuitGame = True
    isOver gameState = False
    viewMessage gameState | gsMessage gameState == NoMessage = ""
                          | gsMessage gameState == SeeOrc = "You see a horrible orc!"
                          | gsMessage gameState == OrcHits = "The orc stabs you with a dagger!"
    viewTile position gameState | cPosition (gsPlayer gameState) == position = '@'
    viewTile position gameState | cPosition (gsOrc gameState) == position = 'o'
    viewTile position GameState { gsMap = gameMap } = viewTerrain position gameMap
    viewTiles gameState = [(x, y, viewTile (x,y) gameState) | (x,y) <- positionsOnMap $ gsMap gameState]

setPlayer player gameState = gameState { gsPlayer = player, gsMessage = NoMessage }
setOrc orc gameState = gameState { gsOrc = orc, gsMessage = NoMessage }

walkableTerrain position gameMap = terrainAt position gameMap == Floor

occupied :: Position -> GameState -> Bool
occupied position gameState = not $ unoccupied position gameState

unoccupied :: Position -> GameState -> Bool
unoccupied position gameState = (cPosition $ gsOrc gameState) /= position
                              && (cPosition $ gsPlayer gameState) /= position
