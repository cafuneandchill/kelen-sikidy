module Main (main) where

import System.Random (getStdGen)

import qualified Seed.Seed as Seed
import qualified Direction
import qualified Duration
import qualified Art

main :: IO ()
main = do
    gen <- getStdGen
    putStrLn "======\ESC[1;33m antāλōþa \ESC[0m======"
    putStrLn ("\ESC[3m"
        ++ "A Method of Divination"
        ++ "\ESC[0m")
    putStrLn "https://www.terjemar.net/kelen/sikidy.php"
    putStrLn "----------------------"
    putStrLn "Generating the mother seed..."
    let motherSeed = Seed.motherSeedRand gen
    putStrLn "Mother seed:"
    print motherSeed
    putStrLn "Generating the daughter seed..."
    let daughterSeed = Seed.daughterFromMother motherSeed
    putStrLn "Daughter seed:"
    print daughterSeed
    putStrLn "Interpreting the travel direction..."
    let d = Direction.direction motherSeed daughterSeed
    putStrLn "Interpreting the art to study..."
    let a = Art.art daughterSeed
    putStrLn "Interpreting the duration of study..."
    let t = Duration.duration daughterSeed
    putStrLn ("A simple question for"
        ++ "\ESC[1;36m"
        ++ " antāλōþa"
        ++ "\ESC[0m"
        ++ " is which direction to travel.")
    putStrLn ("The current configuration says "
        ++ "\ESC[1;36m"
        ++ maybe "---" Direction.toKelen d
        ++ "\ESC[0m"
        ++ " or to the "
        ++ "\ESC[1m"
        ++ maybe "---" Direction.toEnglish d
        ++ "\ESC[0m"
        ++ "."
        )
    putStrLn ("It also suggests that "
        ++ "\ESC[1;36m"
        ++ maybe "---" Art.toKelen a
        ++ "\ESC[1;39m"
        ++ " (" ++ maybe "---" Art.toEnglish a ++ ")"
        ++ "\ESC[0m"
        ++ " would be a good art to study for the next "
        ++ "\ESC[1m"
        ++ maybe "0" show t
        ++ "\ESC[1;36m"
        ++ " jālūi"
        ++ "\ESC[0m"
        ++ " (weeks).")
    return ()
