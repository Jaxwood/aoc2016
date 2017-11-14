module Main (main) where
  import System.Exit
  import Test.HUnit
  import Day1Tests
  import Day2Tests
  import Day3Tests
  import Day4Tests

  main :: IO ()
  main = do
    day3a <- readFile "c:\\code\\private\\adventofcode2016\\tests\\Day3.csv"
    day4a <- readFile "c:\\code\\private\\adventofcode2016\\tests\\Day4.csv"

    counts2 <- runTestTT (test [
             day1,
             day2,
             day3 day3a,
             day4 day4a
            ])

    if (errors counts2 + failures counts2 == 0)
      then exitSuccess
      else exitFailure
