module Day10 (resulta, resultb) where

  import Data.List
  import Data.Either (rights)
  import Text.Parsec
  import Text.Parsec.String

  type Value = Int
  type Low = Int
  type High = Int
  data Reciever = Bot Int | Output Int deriving (Show)
  data Instruction = Assign Reciever Value | Logic Reciever Low High Reciever deriving (Show)

  resulta :: String -> [Int] -> [Instruction]
  resulta x xs = parseInstructions $ lines x

  resultb :: String -> String
  resultb = id

  parseInstructions :: [String] -> [Instruction]
  parseInstructions = rights . map (parse (choice [parseRecieverInstruction, parseValueInstruction]) "")

  parseRecieverInstruction :: Parser Instruction
  parseRecieverInstruction = do
    _ <- string "bot "
    bot <- many1 digit
    _ <- string " gives low to "
    lowTo <- choice [string "bot", string "output"]
    _ <- space
    low <- many1 digit
    _ <- string " and high to "
    highTo <- choice [string "bot", string "output"]
    _ <- space
    high <- many1 digit
    return $ Logic (if lowTo == "bot" then Bot (read low) else Output (read low)) (read low) (read high) (if highTo == "bot" then Bot (read high) else Output (read high))

  parseValueInstruction :: Parser Instruction
  parseValueInstruction = do
    _ <- string "value "
    v <- many1 digit
    _ <- string " goes to bot "
    b <- many1 digit
    return $ Assign (Bot $ read b) (read v)
