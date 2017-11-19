module Main (main) where
  import System.Exit
  import System.Directory
  import Test.HUnit
  import Day1Tests
  import Day2Tests
  import Day3Tests
  import Day4Tests
  import Day5Tests

  main :: IO ()
  main = do
    dir <- getCurrentDirectory 
    day3a <- readFile (dir ++ "/tests/Day3.csv")
    day4a <- readFile (dir ++ "/tests/Day4.csv")

    counts2 <- runTestTT (test [
             day1,
             day2,
             day3 day3a,
             day4 day4a,
             day5
            ])

    if (errors counts2 + failures counts2 == 0)
      then exitSuccess
      else exitFailure
