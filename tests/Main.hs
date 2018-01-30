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
             day11 day11a
            ])

    if (errors counts2 + failures counts2 == 0)
      then exitSuccess
      else exitFailure
