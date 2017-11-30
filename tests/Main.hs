module Main (main) where
  import System.Exit
  import System.Directory
  import Test.HUnit
  import Day1Tests
  import Day2Tests
  import Day3Tests
  import Day4Tests
  import Day5Tests
  import Day6Tests
  import Day7Tests
  import Day8Tests
  import Day9Tests
  import Day10Tests

  main :: IO ()
  main = do
    dir <- getCurrentDirectory 
    day3a <- readFile (dir ++ "/tests/Day3.csv")
    day4a <- readFile (dir ++ "/tests/Day4.csv")
    day6a <- readFile (dir ++ "/tests/Day6.csv")
    day7a <- readFile (dir ++ "/tests/Day7.csv")
    day7b <- readFile (dir ++ "/tests/Day7b.csv")
    day8a <- readFile (dir ++ "/tests/Day8.csv")
    day9a <- readFile (dir ++ "/tests/Day9.csv")
    day10a <- readFile (dir ++ "/tests/Day10.csv")

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
             day10 day10a
            ])

    if (errors counts2 + failures counts2 == 0)
      then exitSuccess
      else exitFailure
