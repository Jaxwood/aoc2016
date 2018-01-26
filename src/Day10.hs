module Day10 (resulta, resultb) where

  import Text.Parsec
  import Text.Parsec.String

  data Instruction = Value Int Int | Instruction Int Target Target deriving (Show,Eq)
  data Target = Bot Int | Output Int deriving (Show,Eq)

  resulta :: String -> [Instruction]
  resulta s =  map (right . parseInput) $ lines s

  resultb :: String -> String
  resultb = id

  right :: Either ParseError a -> a
  right (Left e) = error $ show e
  right (Right a) = a

  -- parse

  parseInput :: String -> Either ParseError Instruction
  parseInput = parse (choice [try parseValue, parseInstruction]) ""

  parseValue :: Parser Instruction
  parseValue = do
    _ <- string "value "
    a <- many1 digit
    _ <- string " goes to bot "
    b <- many1 digit
    return $ Value (read a) (read b)

  parseInstruction :: Parser Instruction
  parseInstruction = do
    _ <- string "bot "
    a <- many1 digit
    _ <- string " gives low to "
    l <- choice [try $ string "bot", string "output"]
    _ <- space
    lv <- many1 digit
    _ <- string " and high to "
    h <- choice [try $ string "bot", string "output"]
    _ <- space
    hv <- many1 digit
    return $ Instruction (read a) (if l == "bot" then Bot (read lv) else Output (read lv)) (if h == "bot" then Bot (read hv) else Output (read hv))
