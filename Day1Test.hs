module Day1Test where
  import Day1
  import Test.HUnit

  getShortestPathTest = TestCase (assertEqual "R2 R2 R2" 2 (getShortestPath ["R2", "R2", "R2"]))

  tests = TestList [TestLabel "getShortestPathTests" getShortestPathTest]
