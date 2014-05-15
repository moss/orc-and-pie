module Roguelike where

class Roguelike gameState where
    advance :: gameState -> Char -> gameState
    isOver :: gameState -> Bool
    notOver :: gameState -> Bool
    notOver state = not (isOver state)
    viewTile :: Position -> gameState -> Char
    viewTiles :: gameState -> [DisplayTile]
    viewTiles roguelike = [(x, y, viewTile (x,y) roguelike) | x <- columns, y <- rows]

type Position = (Int,Int)
type DisplayTile = (Int,Int,Char)

columns = [0..79]
rows = [0..24]
