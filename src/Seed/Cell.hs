module Seed.Cell
    ( Cell (..)
    , negateC
    , addCells
    , addCellLists
    ) where

import System.Random
    ( Random
    , random
    , randomR
    )

data Cell
    = Black
    | White
    deriving (Enum)

instance Eq Cell where
    Black == Black = True
    White == White = True
    _ == _ = False

instance Show Cell where
    show = show . fromEnum

instance Random Cell where
    random g = case randomR (0,1) g of
        (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
        (r, g') -> (toEnum r, g')

negateC :: Cell -> Cell
negateC Black = White
negateC White = Black

addCells :: Cell -> Cell -> Cell
addCells x y
    | x == y    = Black
    | otherwise = White

addCellLists :: [Cell] -> [Cell] -> [Cell]
addCellLists = zipWith addCells
