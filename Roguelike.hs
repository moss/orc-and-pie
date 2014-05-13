module Roguelike where

class Roguelike a where
    viewTile :: Position -> a -> Char
    viewTiles :: a -> [DisplayTile]
    viewTiles roguelike = [(x, y, viewTile (x,y) roguelike) | x <- columns, y <- rows]

type Position = (Int,Int)
type DisplayTile = (Int,Int,Char)

columns = [0..79]
rows = [0..24]
