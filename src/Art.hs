module Art
    ( Art (..)
    , art
    , toKelen
    ) where

import qualified Seed.Seed as Seed
import qualified Seed.Cell as Cell


{-|
    Types of art that the sikidy divines.
-}
data Art
    = Storytelling
    | Dance
    | Healing
    | VisualDesign

instance Show Art where
    show Storytelling = "storytelling and music"
    show Dance        = "dance and motion"
    show Healing      = "healing and touch"
    show VisualDesign = "visual design"

{-|
    Interpret the art for studying from a daughter seed.
-}
art :: Seed.Seed -> Art
art daughter@(Seed.Seed Seed.Daughter _) = getArt . artCellList $ daughter
art _ = error "art: Wrong seed variant or malformed input"

artCellList :: Seed.Seed -> [Cell.Cell]
artCellList seed = [a, b]
  where
    a = Seed.elemS seed (1, 4)
    b = Seed.elemS seed (2, 4)

getArt :: [Cell.Cell] -> Art
getArt cells
    | x == "00" = Healing
    | x == "01" = Storytelling
    | x == "10" = Dance
    | x == "11" = VisualDesign
    | otherwise = error "getArt: Invalid input"
  where
    x = concatMap show cells

toKelen :: Art -> String
toKelen Healing      = "anālte"
toKelen Storytelling = "ansāla"
toKelen Dance        = "antēnnara"
toKelen VisualDesign = "ankeīlke"
