module Day5 (resulta, resultb) where

  import Data.Char
  import Data.Function
  import Data.List
  import Data.Maybe
  import qualified Data.ByteString.Char8 as C
  import Crypto.Hash

  resulta :: String -> String
  resulta s = take 8 $ (password . justs . (generateMD5 isValid')) s

  resultb :: String -> String
  resultb s = order . take 8 $ (nubBy (\a b -> fst a == fst b) . map toTuple . map (drop 5 . fromJust) . justs . (generateMD5 isValid)) s

  md5 :: C.ByteString -> Digest MD5
  md5 = hash

  justs :: [Maybe String] -> [Maybe String]
  justs = filter isJust

  isValid :: String -> Maybe String
  isValid s@('0':'0':'0':'0':'0':p:c:xs)
    | (digitToInt p) <= 7 = Just s
    | otherwise = Nothing
  isValid _ = 
    Nothing

  isValid' :: String -> Maybe String
  isValid' s@('0':'0':'0':'0':'0':xs) =
    Just s
  isValid' _ = 
    Nothing

  order :: [(Int, Char)] -> String
  order s =  map snd $ sortBy (compare `on` fst) s

  toTuple :: String -> (Int, Char)
  toTuple s = (digitToInt $ head s, head $ tail s)

  password :: [Maybe String] -> String
  password = map (head . drop 5 . fromJust)

  generateMD5 :: (String -> Maybe String) ->  String -> [Maybe String]
  generateMD5 fn s = map (fn . show . md5 . C.pack . (++) s . show) [0..]
