module Renderable where

class Renderable a where
    viewTile :: Position -> a -> Char
    viewTiles :: a -> [DisplayTile]
    viewTiles renderable = [(x, y, viewTile (x,y) renderable) | x <- columns, y <- rows]

type Position = (Int,Int)
type DisplayTile = (Int,Int,Char)

columns = [0..79]
rows = [0..24]
