module Art
    ( Art (..)
    , art
    , toEnglish
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
    deriving Show

{-|
    Interpret the art for studying from a daughter seed.
-}
art :: Seed.Seed -> Maybe Art
art = maybe Nothing getArt . artCellList

artCellList :: Seed.Seed -> Maybe [Cell.Cell]
artCellList daughter@(Seed.Seed Seed.Daughter _)
    = Just [a, b]
  where
    a = Seed.elemS daughter (1, 4)
    b = Seed.elemS daughter (2, 4)
artCellList _ = Nothing

getArt :: [Cell.Cell] -> Maybe Art
getArt cells
    | x == "00" = Just Healing
    | x == "01" = Just Storytelling
    | x == "10" = Just Dance
    | x == "11" = Just VisualDesign
    | otherwise = Nothing
  where
    x = concatMap show cells

toEnglish :: Art -> String
toEnglish Healing      = "healing and touch"
toEnglish Storytelling = "storytelling and music"
toEnglish Dance        = "dance and motion"
toEnglish VisualDesign = "visual design"

toKelen :: Art -> String
toKelen Healing      = "anālte"
toKelen Storytelling = "ansāla"
toKelen Dance        = "antēnnara"
toKelen VisualDesign = "ankeīlke"
