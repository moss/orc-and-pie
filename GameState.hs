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

-- | A GameState is viewable as a Roguelike
-- >>> let gs = newGame
-- >>> viewTile (42, 9) gs
-- '@'
-- >>> viewTile (37, 14) gs
-- 'o'
-- >>> viewTile (35, 7) gs
-- '.'
instance Roguelike GameState where
    isOver QuitGame = True
    isOver gameState = False
    viewMessage GameState { gsMessage=m }
                  | m == NoMessage = ""
                  | m == SeeOrc = "You see a horrible orc!"
                  | m == OrcHits = "The orc stabs you with a dagger!"
    viewStatus gameState = "HP: " ++ (show $ cHitPoints $ gsPlayer gameState)
    viewTile position gameState
           | position == playerPosition gameState = '@'
           | position == orcPosition gameState = 'o'
           | True = viewTerrain position $ gsMap gameState
    viewTiles gameState = [(x, y, viewTile (x,y) gameState) | (x,y) <- positionsOnMap $ gsMap gameState]

setPlayer player gameState = gameState { gsPlayer = player, gsMessage = NoMessage }
setOrc orc gameState = gameState { gsOrc = orc, gsMessage = NoMessage }

playerPosition = cPosition . gsPlayer
orcPosition = cPosition . gsOrc

walkableTerrain position gameMap = terrainAt position gameMap == Floor

occupied :: Position -> GameState -> Bool
occupied position gameState = not $ unoccupied position gameState

unoccupied :: Position -> GameState -> Bool
unoccupied position gameState = (orcPosition gameState) /= position
                              && (playerPosition gameState) /= position
