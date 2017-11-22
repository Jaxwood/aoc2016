module Day7 (resulta, resultb, palindrome, toIPv7, IPv7(IPv7)) where

  import Data.List
  import Data.Function
  import Text.Parsec
  import Text.Parsec.String

  type Hypernet = String
  type ABBA = String
  data IPv7 = IPv7 ABBA Hypernet ABBA deriving (Show, Eq)

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
  verify (Right (IPv7 a b c)) = all id [palindrome a || palindrome c, not (palindrome b)]
  verify (Left _) = error "parse error"

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
