module Direction
    ( Direction (..)
    , direction
    , toKelen
    ) where

import Data.Array
    ( Array
    , listArray
    , assocs
    )
import Numeric.Natural (Natural)

import qualified Seed.Cell as Cell
import qualified Seed.Seed as Seed

{-|
    Travel directions that could be interpreted by the sikidy.
-}
data Direction
    = SouthEast 
    | South 
    | SouthWest 
    | West 
    | East 
    | NorthEast 
    | North 
    | NorthWest

instance Show Direction where
    show SouthEast = "south-east"
    show South     = "south"
    show SouthWest = "south-west"
    show West      = "west"
    show East      = "east"
    show NorthEast = "north-east"
    show North     = "north"
    show NorthWest = "north-west"

{-| 
    Interpret the travel direction from a pair of a mother seed
    and a daughter seed.
-}
direction :: Seed.Seed -> Seed.Seed -> Direction
direction
    mother@(Seed.Seed Seed.Mother _)
    daughter@(Seed.Seed Seed.Daughter _)
    --
    = getDirection
    . map toElems
    . filter firstRow
    . assocs
    . directionCellArray mother
    $ daughter
  where
    firstRow :: ((Natural, Natural), e) -> Bool
    firstRow p = (fst . fst $ p) == 1

    toElems :: ((i, i), e) -> e
    toElems ((_, _), e) = e
direction _ _ = error "direction: Invalid input"

directionCellArray
    :: Seed.Seed
    -> Seed.Seed
    -> Array (Natural, Natural) Cell.Cell
directionCellArray m d = listArray ((1,1),(3,4)) (concat [x, y, z])
  where
    x = addColumnCells (d, 5) (m, 3)
    y = addColumnCells (d, 4) (m, 2)
    z = addColumnCells (d, 1) (d, 7)

    addColumnCells
        :: (Seed.Seed, Natural)
        -> (Seed.Seed, Natural)
        -> [Cell.Cell]
    addColumnCells (s1, c1) (s2, c2) = Cell.addCellLists 
        (Seed.columnS s1 c1)
        (Seed.columnS s2 c2)

getDirection :: [Cell.Cell] -> Direction
getDirection cells
    | x `elem` ["0000", "1111"] = SouthEast
    | x `elem` ["0001", "1110"] = South
    | x `elem` ["0010", "1101"] = SouthWest
    | x `elem` ["0011", "1100"] = West
    | x `elem` ["0100", "1011"] = East
    | x `elem` ["0101", "1010"] = NorthEast
    | x `elem` ["0110", "1001"] = North
    | x `elem` ["0111", "1000"] = NorthWest
    | otherwise                 = error "getDirection: Invalid array"
  where
    x :: [Char]
    x = concatMap show cells

toKelen :: Direction -> String
toKelen SouthEast = "r??l??tie"
toKelen South     = "r????lkie"
toKelen SouthWest = "r??h??wie"
toKelen West      = "r????rrie"
toKelen East      = "r??nnie"
toKelen NorthEast = "r????????ie"
toKelen North     = "r??s??rie"
toKelen NorthWest = "r??t??rie"
