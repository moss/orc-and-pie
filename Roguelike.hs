module Roguelike where

class Roguelike gameState where
    advance :: gameState -> Char -> gameState
    isOver :: gameState -> Bool
    notOver :: gameState -> Bool
    notOver = not . isOver
    viewTile :: Position -> gameState -> Char
    viewTiles :: gameState -> [DisplayTile]
    viewTiles roguelike = [(x, y, viewTile (x,y) roguelike) | x <- columns, y <- rows]

type Position = (Int,Int)
type Offset = Position
type DisplayTile = (Int,Int,Char)

columns = [0..79]
rows = [0..24]

offsetBy (xoffset,yoffset) (x,y) = (x+xoffset,y+yoffset)

up :: Offset
up = (0,-1)

left :: Offset
left = (-1,0)

down :: Offset
down = (0,1)

right :: Offset
right = (1,0)
