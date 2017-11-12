module Main (main) where
  import System.Exit
  import Test.HUnit
  import Day1Tests
  import Day2Tests

  main :: IO ()
  main = do
    counts2 <- runTestTT (test [
             day1,
             day2
            ])

    if (errors counts2 + failures counts2 == 0)
      then exitSuccess
      else exitFailure
