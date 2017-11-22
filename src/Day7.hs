module Day7 (resulta, resultb, palindrome) where

  import Data.List
  import Data.Function
  import Text.Parsec
  import Text.Parsec.String

  type Hypernet = String
  type ABBA = String
  data IPv7 = IPv7 ABBA Hypernet ABBA

  resulta :: String -> String
  resulta = id

  resultb :: String -> String
  resultb = id

  palindrome :: String -> Bool
  palindrome s@(a:b:c:d:xs)
    | a == d && b == c && a /= b && c /= d = True
    | otherwise = palindrome $ tail s
  palindrome _ =
    False

  toIPv7 :: String -> Either ParseError IPv7
  toIPv7 = parse match ""

  match :: Parser IPv7
  match = do
    ab <- many1 letter
    _ <- char '['
    hypernet <- many1 letter
    _ <- char ']'
    ba <- many1 letter
    return $ IPv7 ab hypernet ba
