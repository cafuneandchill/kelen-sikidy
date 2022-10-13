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
duration :: Seed.Seed -> Maybe Natural
duration daughter@(Seed.Seed Seed.Daughter _)
    = getDuration
    . durationCellList
    $ daughter
duration _ = Nothing

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

getDuration :: [Cell.Cell] -> Maybe Natural
getDuration cells
    | x `elem` ["0000", "1111"] = Just 2
    | x `elem` ["0001", "1110"] = Just 3
    | x `elem` ["0010", "1101"] = Just 4
    | x `elem` ["0011", "1100"] = Just 5
    | x `elem` ["0100", "1011"] = Just 6
    | x `elem` ["0101", "1010"] = Just 7
    | x `elem` ["0110", "1001"] = Just 8
    | x `elem` ["0111", "1000"] = Just 9
    | otherwise                 = Nothing
  where
    x :: [Char]
    x = concatMap show cells
