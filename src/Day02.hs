module Day02 (resulta, resultb) where

  data Move = U | D | L | R deriving (Eq, Show)

  resulta :: String -> String
  resulta = reverse . digits (digit number) '5' [] . map parse . map split . lines

  resultb :: String -> String
  resultb = reverse . digits (digit char) '5' [] . map parse . map split . lines

  digit :: (Char -> Move -> Char) -> [Move] -> Char -> Char
  digit fn ms start = foldl fn start ms

  digits :: ([Move] -> Char -> Char) -> Char -> [Char] -> [[Move]] -> [Char]
  digits fn start state [] = state

  digits fn start state (x:xs) =
    let n = fn x start
    in digits fn n (n:state) xs 

  split :: String -> [Char]
  split = map id

  parse :: [Char] -> [Move]
  parse = map parseChar

  parseChar :: Char -> Move
  parseChar 'D' = D
  parseChar 'L' = L
  parseChar 'R' = R
  parseChar 'U' = U

  number :: Char -> Move -> Char
  number '1' D = '4'
  number '1' R = '2'
  number '1' _ = '1'

  number '2' U = '2'
  number '2' D = '5'
  number '2' R = '3'
  number '2' L = '1'

  number '3' D = '6'
  number '3' L = '2'
  number '3' _ = '3'

  number '4' U = '1'
  number '4' D = '7'
  number '4' R = '5'
  number '4' L = '4'

  number '5' U = '2'
  number '5' D = '8'
  number '5' R = '6'
  number '5' L = '4'

  number '6' U = '3'
  number '6' D = '9'
  number '6' R = '6'
  number '6' L = '5'

  number '7' U = '4'
  number '7' R = '8'
  number '7' _ = '7'

  number '8' U = '5'
  number '8' D = '8'
  number '8' R = '9'
  number '8' L = '7'

  number '9' U = '6'
  number '9' L = '8'
  number '9' _ = '9'

  char :: Char -> Move -> Char
  char '1' D = '3'
  char '1' _ = '1'

  char '2' R = '3'
  char '2' D = '6'
  char '2' _ = '2'

  char '3' U = '1'
  char '3' D = '7'
  char '3' R = '4'
  char '3' L = '2'

  char '4' L = '3'
  char '4' D = '8'
  char '4' _ = '4'

  char '5' R = '6'
  char '5' _ = '5'

  char '6' U = '2'
  char '6' D = 'A'
  char '6' R = '7'
  char '6' L = '5'

  char '7' R = '8'
  char '7' L = '6'
  char '7' U = '3'
  char '7' D = 'B'

  char '8' U = '4'
  char '8' D = 'C'
  char '8' L = '7'
  char '8' R = '9'

  char '9' L = '8'
  char '9' _ = '9'

  char 'A' U = '6'
  char 'A' R = 'B'
  char 'A' _ = 'A'

  char 'B' U = '7'
  char 'B' D = 'D'
  char 'B' L = 'A'
  char 'B' R = 'C'

  char 'C' U = '8'
  char 'C' L = 'B'
  char 'C' _ = 'C'

  char 'D' U = 'B'
  char 'D' _ = 'D'
