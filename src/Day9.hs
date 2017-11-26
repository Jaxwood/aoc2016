module Day9 (resulta, resultb) where

  import Data.Char (digitToInt)
  import Data.List
  import Text.Parsec
  import Text.Parsec.String

  data Instruction = Instruction Int Int

  resulta :: String -> Int
  resulta = sum . map (length . match') . lines

  resultb :: String -> Int
  resultb = length

  match' :: String -> String
  match' [] =
    ""
  match' ('(':xs) =
    let r = take a' xs'
    in (concat $ take b' $ repeat r) ++ (match' (drop a' xs'))
    where
      (Instruction a' b') = parseField xs
      xs' = tail $ dropWhile ((/=)')') xs
  match' (x:xs) =
    x:(match' xs)

  parseField :: String -> Instruction
  parseField xs =
    case (parse parseInstruction "" xs)
      of (Right a) ->  a
         (Left _) -> error "parse error"

  parseInstruction :: Parser Instruction
  parseInstruction = do
    a <- many1 digit
    _ <- char 'x'
    b <- many1 digit
    return $ Instruction (read a) (read b)
