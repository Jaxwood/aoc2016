module Main (main) where
  import System.Exit
  import System.Directory
  import Test.HUnit
  import Day01Tests
  import Day02Tests
  import Day03Tests
  import Day04Tests
  import Day05Tests
  import Day06Tests
  import Day07Tests
  import Day08Tests
  import Day09Tests
  import Day10Tests
  import Day11Tests
  import Day12Tests
  import Day13Tests
  import Day14Tests
  import Day15Tests
  import Day16Tests
  import Day17Tests
  import Day18Tests
  import Day19Tests
  import Day20Tests
  import Day21Tests
  import Day22Tests
  import Day22 (debug)

  main :: IO ()
  main = do
    dir <- getCurrentDirectory 
    day3a <- readFile (dir ++ "/tests/Day03.csv")
    day4a <- readFile (dir ++ "/tests/Day04.csv")
    day6a <- readFile (dir ++ "/tests/Day06.csv")
    day7a <- readFile (dir ++ "/tests/Day07.csv")
    day7b <- readFile (dir ++ "/tests/Day07b.csv")
    day8a <- readFile (dir ++ "/tests/Day08.csv")
    day9a <- readFile (dir ++ "/tests/Day09.csv")
    day10a <- readFile (dir ++ "/tests/Day10.csv")
    day11a <- readFile (dir ++ "/tests/Day11.csv")
    day12a <- readFile (dir ++ "/tests/Day12.csv")
    day20a <- readFile (dir ++ "/tests/Day20.csv")
    day21a <- readFile (dir ++ "/tests/Day21.csv")
    day22a <- readFile (dir ++ "/tests/Day22.csv")
    day22b <- readFile (dir ++ "/tests/Day22a.csv")

    counts2 <- runTestTT (test [
             day1,
             day2,
             day3 day3a,
             day4 day4a,
             -- day5
             day6 day6a,
             day7 day7a day7b,
             day8 day8a,
             day9 day9a,
             day10 day10a,
             day11 day11a,
             -- day12 day12a,
             day13,
             -- day14
             day15,
             -- day16,
             -- day17,
             -- day18,
             day19,
             day20 day20a,
             day21 day21a,
             day22 day22a day22b
            ])
  
    -- for debugging purposes
    -- writeFile "output.txt" $ debug day22a

    if (errors counts2 + failures counts2 == 0)
      then exitSuccess
      else exitFailure
