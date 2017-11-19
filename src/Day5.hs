module Day5 (resulta, resultb) where

  import Data.List
  import qualified Data.ByteString.Lazy as LB
  import qualified Data.ByteString.Builder as LBB
  import Crypto.Hash

  resulta :: String -> String
  resulta = password . validate . generateMD5

  resultb :: String -> String
  resultb = id

  md5 :: LB.ByteString -> Digest MD5
  md5 = hashlazy

  isValid :: String -> Bool
  isValid s = isPrefixOf ["0", "0", "0", "0", "0"] $ map pure s

  password :: [String] -> String
  password = map (head . drop 5)

  validate :: [String] -> [String]
  validate s = take 8 $ filter isValid s

  generateMD5 :: String -> [String]
  generateMD5 s = map (show . md5 . LBB.toLazyByteString . LBB.stringUtf8 . (++) s . show) plusOne 

  plusOne :: [Integer]
  plusOne = iterate (+1) 0
