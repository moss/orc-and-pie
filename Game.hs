module Game where

import qualified Data.Map.Lazy as Map

import Roguelike

data GameState = GameState { gsMap :: GameMap
                           , gsPlayer :: Position
                           , gsOrc :: Position
                           }
               | QuitGame
type GameMap = Map.Map Position Terrain
data Terrain = NoTerrain | Floor | Wall
             deriving (Eq)

newGame = GameState { gsMap = mapWithRoom (35,7) (44,16)
                    , gsPlayer = (42, 9)
                    , gsOrc = (37, 14)
                    }

mapWithRoom :: Position -> Position -> GameMap
mapWithRoom (left,top) (right,bottom) = Map.fromList $
    [(pos, Floor) | pos <- positionsInRange (left,top) (right,bottom)]
    ++ [(pos, Wall) | pos <- positionsInRange (left-1,top) (left-1,bottom)]
    ++ [(pos, Wall) | pos <- positionsInRange (right+1,top) (right+1,bottom)]
    ++ [(pos, Wall) | pos <- positionsInRange (left-1,top-1) (right+1,top-1)]
    ++ [(pos, Wall) | pos <- positionsInRange (left-1,bottom+1) (right+1,bottom+1)]

positionsInRange :: Position -> Position -> [Position]
positionsInRange (left,top) (right,bottom) =
    [(x,y) | x <- [left..right], y <- [top..bottom]]

terrainAt :: Position -> GameMap -> Terrain
terrainAt = Map.findWithDefault NoTerrain

instance Roguelike GameState where
    advance gameState inputCharacter = let advancePlayer = getCommand inputCharacter
                                           in (advancePlayer . advanceOrc) gameState

    isOver QuitGame = True
    isOver gameState = False
    viewTile position gameState | gsPlayer gameState == position = '@'
    viewTile position gameState | gsOrc gameState == position = 'o'
    viewTile position GameState { gsMap = gameMap } = viewTerrain position gameMap

viewTerrain position gameMap = case terrainAt position gameMap of
                                 Floor -> '.'
                                 Wall -> viewWall (position,gameMap)
                                 NoTerrain -> ' '

viewWall mapPosition = if isCornerWall mapPosition
                              then '+'
                              else if isVerticalWall mapPosition
                                then '|'
                                else '-'

isCornerWall mapPosition = isVerticalWall mapPosition
                        && isHorizontalWall mapPosition

isVerticalWall mapPosition = wallInDirection up mapPosition
                          || wallInDirection down mapPosition

isHorizontalWall mapPosition = wallInDirection left mapPosition
                            || wallInDirection right mapPosition

wallInDirection offset (position,gameMap) =
    let testPosition = offsetBy offset position in
      terrainAt testPosition gameMap == Wall

keymap = Map.fromList [ ('h', movePlayer left)
                      , ('j', movePlayer down)
                      , ('k', movePlayer up)
                      , ('l', movePlayer right)
                      , ('y', movePlayer $ up `offsetBy` left)
                      , ('u', movePlayer $ up `offsetBy` right)
                      , ('b', movePlayer $ down `offsetBy` left)
                      , ('n', movePlayer $ down `offsetBy` right)
                      , ('q', quitGame)
                      ]

quitGame gameState = QuitGame
doNothing gameState = gameState
getCommand inputCharacter = Map.findWithDefault doNothing inputCharacter keymap

advanceOrc gameState = moveOrc right gameState

setPlayer gameState player = gameState { gsPlayer = player }
setOrc gameState orc = gameState { gsOrc = orc }

moveOrc = moveCharacter gsOrc setOrc
movePlayer = moveCharacter gsPlayer setPlayer

type PositionGetter = GameState -> Position
type PositionSetter = GameState -> Position -> GameState
type GameMove = GameState -> GameState

moveCharacter :: PositionGetter -> PositionSetter -> Position -> GameMove
moveCharacter positionGetter positionSetter offset gameState =
    let newPosition = offsetBy offset (positionGetter gameState) in
      case terrainAt newPosition (gsMap gameState) of
        Floor -> positionSetter gameState newPosition
        _ -> gameState

offsetBy (xoffset,yoffset) (x,y) = (x+xoffset,y+yoffset)

up = (0,-1)
left = (-1,0)
down = (0,1)
right = (1,0)
