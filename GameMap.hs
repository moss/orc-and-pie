module GameMap ( GameMap, Terrain(NoTerrain,Floor,Wall), mapWithRoom, terrainAt, positionsOnMap ) where

import qualified Data.Map.Lazy as Map

import Roguelike

type GameMap = Map.Map Position Terrain
data Terrain = NoTerrain | Floor | Wall
             deriving (Eq, Show)

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

positionsOnMap = Map.keys
