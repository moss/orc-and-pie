module Game where

import Data.List
import qualified Data.Vector as Vector
import qualified Data.Map.Lazy as Map
import System.Random

import GameMap
import Roguelike
import TerrainView

data GameState = GameState { gsMap :: GameMap
                           , gsPlayer :: Position
                           , gsOrc :: Position
                           }
               | QuitGame

newGame = GameState { gsMap = mapWithRoom (35,7) (44,16)
                    , gsPlayer = (42, 9)
                    , gsOrc = (37, 14)
                    }

gameMoves :: RandomGen a => [Char] -> a -> [GameMove]
gameMoves input orcBrain = let playerMoves = map advancePlayer input
                               orcMoves = map advanceOrc $ genOrcMoves orcBrain
                               in (concat.transpose) [playerMoves, orcMoves]

genOrcMoves :: RandomGen a => a -> [Position]
genOrcMoves = randomChoices $ Vector.fromList [up, down, left, right]

randomChoices :: RandomGen a => Vector.Vector b -> a -> [b]
randomChoices options gen = map (Vector.unsafeIndex options) (randomRs (0,3) gen)

advancePlayer inputCharacter = getCommand inputCharacter
advanceOrc direction = moveOrc direction

instance Roguelike GameState where
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
