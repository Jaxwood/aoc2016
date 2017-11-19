module Day5 (resulta, resultb) where

  import Data.Char
  import Data.Function
  import Data.List
  import Data.Maybe
  import qualified Data.ByteString.Char8 as C
  import Crypto.Hash

  resulta :: String -> String
  resulta = password . justs . generateMD5

  resultb :: String -> String
  resultb = order . map toTuple . map (drop 5 . fromJust) . justs . generateMD5

  md5 :: C.ByteString -> Digest MD5
  md5 = hash

  justs :: [Maybe String] -> [Maybe String]
  justs s = take 8 $ filter isJust s

  isValid :: String -> Maybe String
  isValid s
    | isPrefixOf ["0", "0", "0", "0", "0"] $ map pure s = Just s
    | otherwise = Nothing

  order :: [(Int, Char)] -> String
  order s =  map snd $ sortBy (compare `on` fst) s

  toTuple :: String -> (Int, Char)
  toTuple s = (digitToInt $ head s, head $ tail s)

  password :: [Maybe String] -> String
  password = map (head . drop 5 . fromJust)

  generateMD5 :: String -> [Maybe String]
  generateMD5 s = map (isValid . show . md5 . C.pack . (++) s . show) [0..]
