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

  resultb :: String -> Int
  resultb = length . filter ssl . map toIPv7 . lines

  abba :: String -> Bool
  abba s@(a:b:c:d:xs)
    | a == d && b == c && a /= b && c /= d = True
    | otherwise = abba $ tail s
  abba _ =
    False

  aba :: [String] -> String -> Bool
  aba hn s@(a:b:c:xs)
    | a == c && b /= a && b /= c && (any (isInfixOf [b,a,b]) hn) = True
    | otherwise = aba hn (tail s)
  aba _ _ =
    False

  ssl :: Either ParseError IPv7 -> Bool
  ssl (Right (IPv7 a b)) = any (aba b) a
  ssl (Left _) = error "parse error"

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
