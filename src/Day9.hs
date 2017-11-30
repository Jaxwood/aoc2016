{-# LANGUAGE ViewPatterns #-}
module Day9 (resulta, resultb) where

  import Data.List
  import Text.Parsec
  import Text.Parsec.String

  type Pos = Int
  type Val = Int
  data Instruction = Instruction Int Int
  data Token = Marker Int Int Int Int | Letter Char Pos Val deriving (Show)

  resulta :: String -> Int
  resulta = sum . map (length . match) . lines

  resultb :: String -> Int
  resultb = sumLetters . updateValue . tokenize [] 0

  closeMarker :: Char -> Bool
  closeMarker = (/=)')'

  updateValue :: [Token] -> [Token]
  updateValue ((m@(Marker a b c d)):xs) = m:(updateValue $ (map (update m) xs))
  updateValue (x:xs) = x:updateValue xs
  updateValue [] = []

  update :: Token -> Token -> Token
  update (Marker a b c d) l@(Letter c' p v) = if p < d && p > c then (Letter c' p (v*b)) else l
  update (Marker a b c d) m@(Marker _ _ _ _) = m

  sumLetters :: [Token] -> Int
  sumLetters ((Marker _ _ _ _):ms) = 0 + sumLetters ms
  sumLetters ((Letter _ _ v):ms) = v + sumLetters ms
  sumLetters [] = 0

  tokenize :: [Token] -> Int -> String -> [Token]
  tokenize acc _ [] = reverse acc
  tokenize acc i ('(':xs) = let (Instruction a b) = parseField xs
                            in tokenize ((Marker a b i (i'+a)):acc) i' xs'
                            where
                              xs' = tail $ dropWhile closeMarker xs
                              i' = (length $ takeWhile closeMarker xs) + i + 2
  tokenize acc i (x:xs) = tokenize ((Letter x i 1):acc) i' xs
                          where i' = i + 1

  match :: String -> String
  match [] = ""

  match ('(':xs) =
    let r = take a xs'
    in (concat $ take b $ repeat r) ++ (match (drop a xs'))
    where
      (Instruction a b) = parseField xs
      xs' = tail $ dropWhile ((/=)')') xs

  match (x:xs) =
    x:(match xs)

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
