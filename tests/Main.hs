module Main (main) where
  import System.Exit
  import Test.HUnit
  import Day1Tests
  import Day2Tests
  import Day3Tests

  main :: IO ()
  main = do
    day3a <- readFile "c:\\code\\private\\adventofcode2016\\tests\\Day3.csv"

    counts2 <- runTestTT (test [
             day1,
             day2,
             day3 day3a
            ])

    if (errors counts2 + failures counts2 == 0)
      then exitSuccess
      else exitFailure
