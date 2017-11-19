module Day5 (resulta, resultb) where

  import Data.List
  import Data.Maybe
  import qualified Data.ByteString.Char8 as C
  import Crypto.Hash

  resulta :: String -> String
  resulta = password . justs . generateMD5

  resultb :: String -> String
  resultb = id

  md5 :: C.ByteString -> Digest MD5
  md5 = hash

  justs :: [Maybe String] -> [Maybe String]
  justs s = take 8 $ filter isJust s

  isValid :: String -> Maybe String
  isValid s
    | isPrefixOf ["0", "0", "0", "0", "0"] $ map pure s = Just s
    | otherwise = Nothing

  password :: [Maybe String] -> String
  password = map (head . drop 5 . fromJust)

  generateMD5 :: String -> [Maybe String]
  generateMD5 s = map (isValid . show . md5 . C.pack . (++) s . show) [0..]
