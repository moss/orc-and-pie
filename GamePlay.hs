module GamePlay where

import Data.List
import qualified Data.Map.Lazy as Map
import qualified Data.Vector as Vector
import System.Random

import GameMap
import GameState
import Roguelike

type GameMove = GameState -> GameState

gameMoves :: RandomGen a => [Char] -> a -> [GameMove]
gameMoves input orcBrain = let playerMoves = map advancePlayer input
                               orcMoves = map advanceOrc $ randomRs (0,20) orcBrain
                               in (concat.transpose) [playerMoves, orcMoves]

advanceOrc :: Int -> GameMove
advanceOrc d20 | d20 <= 3 = moveOrc up
               | d20 <= 6 = moveOrc down
               | d20 <= 9 = moveOrc left
               | d20 <= 12 = moveOrc right
advanceOrc _ = advanceOrcTowardsPlayer

advancePlayer inputCharacter = getCommand inputCharacter

advanceOrcTowardsPlayer gameState = moveOrc (orcToPlayerDir gameState) gameState

-- | Find the direction from the orc to the player
-- >>> orcToPlayerDir newGame { gsOrc=(1,1), gsPlayer=(10,10) }
-- (1,1)
-- >>> orcToPlayerDir newGame { gsOrc=(10,10), gsPlayer=(1,1) }
-- (-1,-1)
-- >>> orcToPlayerDir newGame { gsOrc=(10,10), gsPlayer=(10,1) }
-- (0,-1)
orcToPlayerDir GameState { gsOrc=(orcx,orcy), gsPlayer=(playerx,playery) } =
    (dampen (playerx-orcx), dampen (playery-orcy))

dampen num | num < 0 = -1
           | num > 0 = 1
           | num == 0 = 0

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

-- | Move the orc by an offset
-- Moves the orc if the target position is free.
-- >>> let orcInTheMiddle = newGame { gsOrc=(40,10) }
-- >>> let moveDownRight = moveOrc (1,1)
-- >>> gsOrc $ moveDownRight orcInTheMiddle
-- (41,11)
--
-- The orc can't walk into walls.
-- >>> let orcByTheWall = newGame { gsOrc=(10,10), gsMap=mapWithRoom (1,1) (10,10) }
-- >>> gsOrc $ moveDownRight orcByTheWall
-- (10,10)
--
-- If the orc walks into the player, it attacks instead of moving.
-- >>> let orcByThePlayer = newGame { gsOrc=(40,10), gsPlayer=(41,11) }
-- >>> gsOrc $ moveDownRight orcByThePlayer
-- (40,10)
-- >>> gsMessage $ moveDownRight orcByThePlayer
-- OrcHits
moveOrc :: Position -> GameMove
moveOrc = moveCharacter gsOrc setOrc attackPlayer

movePlayer = moveCharacter gsPlayer setPlayer doNothing

moveCharacter :: PositionGetter -> PositionSetter -> GameMove -> Position -> GameMove
moveCharacter positionGetter positionSetter occupiedMove offset gameState =
    case offsetBy offset (positionGetter gameState) of
      newPosition | occupied newPosition gameState -> occupiedMove gameState
                  | walkableTerrain newPosition (gsMap gameState) -> positionSetter newPosition gameState
      _ -> gameState

attackPlayer gameState = gameState { gsMessage=OrcHits }
