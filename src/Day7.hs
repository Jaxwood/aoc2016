module Day7 (resulta, resultb) where

  import Data.List
  import Data.Function
  import Text.Parsec
  import Text.Parsec.String

  type Hypernet = String
  type Supernet = String
  data IPv7 = IPv7 [Supernet] [Hypernet] deriving (Show, Eq)

  resulta :: String -> Int
  resulta = length . filter tls . map toIPv7 . lines

  resultb :: String -> String
  resultb = id

  abba :: String -> Bool
  abba s@(a:b:c:d:xs)
    | a == d && b == c && a /= b && c /= d = True
    | otherwise = abba $ tail s
  abba _ =
    False

  tls :: Either ParseError IPv7 -> Bool
  tls (Right (IPv7 a b)) = all id [any abba a, all (not . abba) b]
  tls (Left _) = error "parse error"

  toIPv7 :: String -> Either ParseError IPv7
  toIPv7 = parse match ""

  sub :: (Int -> Bool) -> [String] -> [String]
  sub fn xs = map fst $ filter (fn . snd) $ zip xs [1..]

  match :: Parser IPv7
  match = do
    ab <- sepBy (many letter) $ oneOf ['[', ']']
    return $ IPv7 (sub odd ab) (sub even ab) 
