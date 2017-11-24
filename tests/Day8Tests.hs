module Day8Tests where
  import Test.HUnit
  import Day8 (resulta, resultb, rotate, toInstructions, Instruction(On,Row,Column), row, update)

  day8 :: String -> Test
  day8 a = test [
      assertEqual "day8a" 0 (resulta a),
      assertEqual "day8b" 0 (resultb a),
      assertEqual "rotate" [1,0,0] (rotate [0,0,1] 1),
      assertEqual "rotate" [0,1,0] (rotate [0,0,1] 2),
      assertEqual "rotate" [0,0,1] (rotate [0,0,1] 3),
      assertEqual "rotate" [1,0,0] (rotate [0,0,1] 4),
      assertEqual "rotate" [0,1,0] (rotate [0,0,1] 5),
      assertEqual "rotate" [0,0,1] (rotate [0,0,1] 6),
      assertEqual "rotate" [1,0,0] (rotate [0,0,1] 7),
      assertEqual "instructions" [On 3 2, Column 1 1, Row 0 4, Column 1 1] (toInstructions a),
      assertEqual "row" [1,0,0] (row (On 1 1) [0,0,0]),
      assertEqual "row" [1,1,1] (row (On 1 1) [0,1,1]),
      assertEqual "set update" [[1,1,1,0,0,0], [1,1,1,0,0,0], [0,0,0,0,0,0]] (update [[0,0,0,0,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0]] (On 3 2)),
      assertEqual "rotate row" [[0,0,0,1,0,1], [0,0,0,0,0,0], [0,0,0,0,0,0]] (update [[1,0,1,0,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0]] (Row 0 3)),
      assertEqual "rotate row" [[1,0,1,0,0,0], [0,0,0,0,0,1], [0,0,0,0,0,0]] (update [[1,0,1,0,0,0], [0,0,0,1,0,0], [0,0,0,0,0,0]] (Row 1 2)),
      assertEqual "rotate column" [[0,0,0,0,0,0], [1,0,0,0,0,0], [0,0,0,0,0,0]] (update [[1,0,0,0,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0]] (Column 0 1))
    ]
