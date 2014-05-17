module Game where

import qualified Data.Map.Lazy as Map

import GameMap
import Roguelike
import TerrainView

data GameState = GameState { gsMap :: GameMap
                           , gsPlayer :: Position
                           , gsOrc :: Position
                           , gsOrcMoves :: [Position]
                           }
               | QuitGame

newGame = GameState { gsMap = mapWithRoom (35,7) (44,16)
                    , gsPlayer = (42, 9)
                    , gsOrc = (37, 14)
                    , gsOrcMoves = cycle [ right, right, right, right
                                         , down, down
                                         , left, left, left, left
                                         , up, up
                                         ]
                    }

instance Roguelike GameState where
    advance gameState inputCharacter = let advancePlayer = getCommand inputCharacter
                                           in (advancePlayer . advanceOrc) gameState

    isOver QuitGame = True
    isOver gameState = False
    viewTile position gameState | gsPlayer gameState == position = '@'
    viewTile position gameState | gsOrc gameState == position = 'o'
    viewTile position GameState { gsMap = gameMap } = viewTerrain position gameMap

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

advanceOrc gameState@GameState { gsOrcMoves = firstMove:restMoves } =
    moveOrc firstMove gameState { gsOrcMoves = restMoves }

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
