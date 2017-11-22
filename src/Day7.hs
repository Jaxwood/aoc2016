module Day7 (resulta, resultb) where

  import Data.List
  import Data.Function
  import Text.Parsec
  import Text.Parsec.String

  type Hypernet = String
  type ABBA = String
  data IPv7 = IPv7 [ABBA] [Hypernet] deriving (Show, Eq)

  resulta :: String -> Int
  resulta = length . filter verify . map toIPv7 . lines

  resultb :: String -> String
  resultb = id

  palindrome :: String -> Bool
  palindrome s@(a:b:c:d:xs)
    | a == d && b == c && a /= b && c /= d = True
    | otherwise = palindrome $ tail s
  palindrome _ =
    False

  verify :: Either ParseError IPv7 -> Bool
  verify (Right (IPv7 a b)) = all id [any palindrome a, all (not . palindrome) b]
  verify (Left _) = error "parse error"

  toIPv7 :: String -> Either ParseError IPv7
  toIPv7 = parse match ""

  sub :: (Int -> Bool) -> [String] -> [String]
  sub fn xs = map fst $ filter (fn . snd) $ zip xs [1..]

  match :: Parser IPv7
  match = do
    ab <- sepBy (many letter) $ oneOf ['[', ']']
    return $ IPv7 (sub odd ab) (sub even ab) 
