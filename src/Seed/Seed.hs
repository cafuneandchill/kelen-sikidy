module Seed.Seed
    ( Seed (..)
    , MotherOrDaughter (..)
    , motherSeedRand
    , motherSeed
    , daughterFromMother
    , elemS
    , rowS
    , columnS
    , transposeS
    ) where

import Data.Array
    ( Array
    , bounds
    , (!)
    , listArray
    , assocs
    , ixmap
    )
import Data.Ix (Ix)
import Data.List (transpose)
import Data.Tuple (swap)
import Numeric.Natural (Natural)
import System.Random
    ( RandomGen
    , randoms
    )

import qualified Seed.Cell as Cell

data Seed = Seed MotherOrDaughter (Array (Natural, Natural) Cell.Cell)

instance Show Seed where
    show (Seed _ arr) = unlines
        . flip map [i0 .. iN]
        $ (\ i -> unwords
            . map (show . arrElem i)
            $ [j0 .. jN]
        )
      where
        arrElem i j = arr ! (i, j)
        boundsElem f g = g . f $ bounds arr
        i0 = boundsElem fst fst
        iN = boundsElem snd fst
        j0 = boundsElem fst snd
        jN = boundsElem snd snd

instance Eq Seed where
    Seed kind1 arr1 == Seed kind2 arr2
        | (kind1 == kind2) && (arr1 == arr2) = True
        | otherwise                          = False

data MotherOrDaughter
    = Mother
    | Daughter

instance Eq MotherOrDaughter where
    Mother == Mother = True
    Daughter == Daughter = True
    _ == _ = False

data Vector
    = Row
    | Column

{-|
    Generates a random mother seed.
-}
motherSeedRand :: RandomGen g => g -> Seed
motherSeedRand g = motherSeed (randomCells g)
  where
    randomCells :: RandomGen g => g -> [Cell.Cell]
    randomCells = take (4*4) . randoms

{-|
    The constructor for a mother seed --
    a 4x4 array containing `Cell` values.
-}
motherSeed :: [Cell.Cell] -> Seed
motherSeed = listSeed Mother (4, 4)

{-|
    Constructs a daughter seed from a mother seed.

    Uses a special algorithm, described in @sikidy-algorithm.md@.
-}
daughterFromMother :: Seed -> Seed
daughterFromMother (Seed Daughter _) = error
    $ "daughterFromMother: Can't create a daughter seed"
    ++ " from a daughter seed"
daughterFromMother seed@(Seed Mother _) = daughterSeed
    $ concat
    . transpose
    $ [i, m, j, p, k, n, l, q]
  where
    row' :: Natural -> [Cell.Cell]
    row' = rowS seed

    column' :: Natural -> [Cell.Cell]
    column' = columnS seed

    i = Cell.addCellLists (row' 4) (row' 3)
    j = Cell.addCellLists (row' 2) (row' 1)
    k = Cell.addCellLists (column' 1) (column' 2)
    l = Cell.addCellLists (column' 3) (column' 4)
    m = Cell.addCellLists i j
    n = Cell.addCellLists k l
    p = Cell.addCellLists m n
    q = Cell.addCellLists p (column' 4)

daughterSeed :: [Cell.Cell] -> Seed
daughterSeed = listSeed Daughter (4, 8)

transposeS :: Seed -> Seed
transposeS (Seed generation arr) = Seed generation (transposeArray arr)
  where
    transposeArray :: (Ix i) => Array (i, i) e -> Array (i, i) e
    transposeArray arr' = ixmap (bounds arr') swap arr'

listSeed :: MotherOrDaughter -> (Natural, Natural) -> [Cell.Cell] -> Seed
listSeed generation (rows, columns) cells = Seed generation
    $ listArray
    ( (1, 1)
    , (rows, columns)
    ) cells

elemS :: Seed -> (Natural, Natural) -> Cell.Cell
elemS (Seed _ arr) = (arr !)

rowS :: Seed -> Natural -> [Cell.Cell]
rowS = getVector Row

columnS :: Seed -> Natural -> [Cell.Cell]
columnS = getVector Column

getVector :: Vector -> Seed -> Natural -> [Cell.Cell]
getVector v (Seed _ arr) n = map getElement $ filter (isIndex v n) (assocs arr)
  where
    getElement :: ((i, j), e) -> e
    getElement = snd

    isIndex :: Eq i => Vector -> i -> ((i, i), e) -> Bool
    isIndex Row x = (x ==) . fst . fst
    isIndex Column x = (x ==) . snd . fst
