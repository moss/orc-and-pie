module Game where

import qualified Data.Map.Lazy as Map

import Roguelike

data GameState = GameState { gsMap :: GameMap, gsPlayer :: Position }
               | QuitGame
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

instance Roguelike GameState where
    advance gameState inputCharacter = getCommand inputCharacter gameState
    isOver QuitGame = True
    isOver gameState = False
    viewTile position gameState | gsPlayer gameState == position = '@'
    viewTile position GameState { gsMap = gameMap } = viewTerrain position gameMap

viewTerrain position gameMap = case terrainAt position gameMap of
                                 Floor -> '.'
                                 NoTerrain -> ' '

keymap = Map.fromList [ ('h', movePlayer left)
                      , ('j', movePlayer down)
                      , ('k', movePlayer up)
                      , ('l', movePlayer right)
                      , ('q', quitGame)
                      ]

quitGame gameState = QuitGame
doNothing gameState = gameState
getCommand inputCharacter = Map.findWithDefault doNothing inputCharacter keymap

movePlayer :: Position -> GameState -> GameState
movePlayer offset gameState = gameState { gsPlayer = offsetBy offset (gsPlayer gameState) }

offsetBy (xoffset,yoffset) (x,y) = (x+xoffset,y+yoffset)

up = (0,-1)
left = (-1,0)
down = (0,1)
right = (1,0)
