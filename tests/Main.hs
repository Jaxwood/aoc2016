module Main (main) where
  import System.Exit
  import Test.HUnit
  import ParseTests

  main :: IO ()
  main = do
    counts2 <- runTestTT (test [
             parseTest 
            ])

    if (errors counts2 + failures counts2 == 0)
      then exitSuccess
      else exitFailure