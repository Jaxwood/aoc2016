module Day7Tests where
  import Test.HUnit
  import Data.Either
  import Day7 (resulta, resultb, palindrome, toIPv7, IPv7(IPv7), verify)

  day7 :: String -> Test
  day7 str = test [
      assertEqual "day7a" 3 (resulta str),
      -- assertEqual "day7b" "" (resultb str),
      assertEqual "parse" (Right (IPv7 ["dqpthtgufgzjojuvzvm", "iingwezvcbtowwzc", "wcucfmnlarrvdceuxqc", "klxammurpqgmpsxsr"] ["eejdhpcqyiydwod", "uzlxaqenhgsebqskn", "dkwcsxeitcobaylhbvc"])) (toIPv7 "dqpthtgufgzjojuvzvm[eejdhpcqyiydwod]iingwezvcbtowwzc[uzlxaqenhgsebqskn]wcucfmnlarrvdceuxqc[dkwcsxeitcobaylhbvc]klxammurpqgmpsxsr")
    ]
