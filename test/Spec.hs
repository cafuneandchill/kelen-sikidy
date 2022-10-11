import Data.Array (listArray)
import Test.HUnit
    ( Test (..)
    , assertEqual
    )
import Test.HUnit.Text (runTestTTAndExit)

import qualified Seed.Seed as Seed
import qualified Seed.Cell as Cell

main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = TestList [test1, test2]

-- test Seed.motherSeed
test1 :: Test
test1 = TestLabel "Seed.motherSeed"
    $ TestCase (assertEqual
    "Seed.motherSeed $ replicate 16 Cell.Black"
    ( Seed.motherSeed
    $ replicate 16 Cell.Black
    )
    ( Seed.Seed Seed.Mother 
    . listArray ((1,1),(4,4))
    $ replicate 16 Cell.Black
    )
    )

-- test Seed.daughterFromMother
test2 :: Test
test2 = TestLabel "Seed.daughterFromMother"
    $ TestCase (assertEqual
    ( "Seed.daughterFromMother"
    ++ " . Seed.motherSeed $ replicate 16 Cell.Black"
    )
    ( Seed.daughterFromMother
    . Seed.motherSeed
    . replicate 16
    $ Cell.Black
    )
    ( Seed.Seed Seed.Daughter
    . listArray ((1,1),(4,8))
    . replicate 32
    $ Cell.Black
    )
    )
