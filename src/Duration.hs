module Duration
    ( duration
    ) where

import Data.List (foldl1')
import Numeric.Natural (Natural)

import qualified Seed.Seed as Seed
import qualified Seed.Cell as Cell

{-|
    Interpret the duration of studying an `Art` from a daughter seed.
-}
duration :: Seed.Seed -> Natural
duration daughter@(Seed.Seed Seed.Daughter _)
    = getDuration
    . durationCellList
    $ daughter
duration _ = error "duration: Invalid input"

durationCellList :: Seed.Seed -> [Cell.Cell]
durationCellList seed = [a, b, c, d]
  where
    sumC :: [Cell.Cell] -> Cell.Cell
    sumC = foldl1' Cell.addCells

    e :: Natural -> Natural -> Cell.Cell
    e i j = Seed.elemS seed (i, j)

    a = sumC
        [ e 1 1
        , e 2 2
        , e 3 2
        , e 4 1]
    b = sumC
        [ e 1 4
        , e 2 3
        , e 3 3
        , e 4 4]
    c = sumC
        [ e 1 5
        , e 2 6
        , e 3 6
        , e 4 5]
    d = sumC
        [ e 1 8
        , e 2 7
        , e 3 7
        , e 4 8]

getDuration :: [Cell.Cell] -> Natural
getDuration cells
    | x `elem` ["0000", "1111"] = 2
    | x `elem` ["0001", "1110"] = 3
    | x `elem` ["0010", "1101"] = 4
    | x `elem` ["0011", "1100"] = 5
    | x `elem` ["0100", "1011"] = 6
    | x `elem` ["0101", "1010"] = 7
    | x `elem` ["0110", "1001"] = 8
    | x `elem` ["0111", "1000"] = 9
    | otherwise               = error "getDuration: Invalid input"
  where
    x :: [Char]
    x = concatMap show cells
