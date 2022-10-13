module Direction
    ( Direction (..)
    , direction
    , toEnglish
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
    deriving Show

{-| 
    Interpret the travel direction from a pair of a mother seed
    and a daughter seed.
-}
direction :: Seed.Seed -> Seed.Seed -> Maybe Direction
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
direction _ _ = Nothing

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

getDirection :: [Cell.Cell] -> Maybe Direction
getDirection cells
    | x `elem` ["0000", "1111"] = Just SouthEast
    | x `elem` ["0001", "1110"] = Just South
    | x `elem` ["0010", "1101"] = Just SouthWest
    | x `elem` ["0011", "1100"] = Just West
    | x `elem` ["0100", "1011"] = Just East
    | x `elem` ["0101", "1010"] = Just NorthEast
    | x `elem` ["0110", "1001"] = Just North
    | x `elem` ["0111", "1000"] = Just NorthWest
    | otherwise                 = Nothing
  where
    x :: [Char]
    x = concatMap show cells

toEnglish :: Direction -> String
toEnglish SouthEast = "south-east"
toEnglish South     = "south"
toEnglish SouthWest = "south-west"
toEnglish West      = "west"
toEnglish East      = "east"
toEnglish NorthEast = "north-east"
toEnglish North     = "north"
toEnglish NorthWest = "north-west"

toKelen :: Direction -> String
toKelen SouthEast = "rālātie"
toKelen South     = "rāēlkie"
toKelen SouthWest = "rāhāwie"
toKelen West      = "rāōrrie"
toKelen East      = "rānnie"
toKelen NorthEast = "rāþīñie"
toKelen North     = "rāsīrie"
toKelen NorthWest = "rātārie"
