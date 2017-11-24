module Day8 (resulta, resultb, rotate, toInstructions, Instruction(On,Row,Column), row, update) where

  import Data.Either (rights)
  import Data.List
  import Text.Parsec
  import Text.Parsec.String

  data Instruction = On Int Int | Row Int Int | Column Int Int deriving (Eq, Show)

  type Screen = [[Int]]

  rect :: Screen
  rect = concat [take 6 $ repeat x | x <- [take 50 $ repeat 0]]

  resulta :: String -> Int
  resulta s = 0

  resultb :: String -> Int
  resultb s = 0

  rotate :: [Int] -> Int -> [Int]
  rotate xs i
    | i > ln = rotate' xs $ ln - m
    | otherwise = rotate' xs $ ln - i
    where ln = length xs
          m = i `mod` ln

  rotate' :: [Int] -> Int -> [Int]
  rotate' xs i =
    let (a, b) = splitAt i xs
    in b ++ a

  update :: [[Int]] -> Instruction -> [[Int]]
  update ss i@(On x y) =
    map ((\(a, b) -> if b < y then row i a else a)) $ map id $ zip ss [0..]

  update ss i@(Row x y) =
    map ((\(a, b) -> if b == x then rotate a y else a)) $ map id $ zip ss [0..]

  update ss i@(Column x y) =
    transpose $ map ((\(a, b) -> if a == x then rotate b y else b)) $ map id $ zip [0..] $ transpose ss

  row :: Instruction -> [Int] -> [Int]
  row (On x y) s =
    map (\(a, b) -> if b < x then 1 else a) $ zip s [0..]
    
  row (Column x y) s = s

  row (Row x y) s = s

  toInstructions :: String -> [Instruction]
  toInstructions = rights . map (parse (choice [try matchOn, try matchRow, matchColumn]) "") . lines

  matchOn :: Parser Instruction
  matchOn = do
    _ <- string "rect"
    _ <- space
    a <- many digit
    _ <- char 'x'
    b <- many digit
    return $ On (read a) (read b)

  matchRow :: Parser Instruction
  matchRow = do
    _ <- string "rotate"
    _ <- space
    _ <- string "row"
    _ <- space
    _ <- string "y="
    a <- many digit
    _ <- space
    _ <- string "by"
    _ <- space
    b <- many digit
    return $ Row (read a) (read b)

  matchColumn :: Parser Instruction
  matchColumn = do
    _ <- string "rotate"
    _ <- space
    _ <- string "column"
    _ <- space
    _ <- string "x="
    a <- many digit
    _ <- space
    _ <- string "by"
    _ <- space
    b <- many digit
    return $ Column (read a) (read b)

