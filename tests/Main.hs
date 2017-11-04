module Main (main) where
  import System.Exit
  import Test.HUnit
  import Day1 (getShortestPath)

  main :: IO ()
  main = do
    let getShortestPathTests = TestCase (assertEqual "R2 R2 R2" 2 (getShortestPath "R2, R2, R2"))
    counts2 <- runTestTT (test [
              getShortestPathTests
            ])

    if (errors counts2 + failures counts2 == 0)
      then exitSuccess
      else exitFailure