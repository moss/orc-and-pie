module TerrainView ( viewTerrain ) where

import GameMap
import Roguelike

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
