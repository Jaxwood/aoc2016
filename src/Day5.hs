module Day5 (resulta, resultb) where

  import Data.List
  import qualified Data.ByteString.Lazy as LB
  import qualified Data.ByteString.Builder as LBB
  import Crypto.Hash

  resulta :: String -> String
  resulta = password . validate . generateMD5

  resultb :: String -> Int
  resultb str = 0

  md5 :: LB.ByteString -> Digest MD5
  md5 = hashlazy

  isValid :: String -> Bool
  isValid s = isPrefixOf (take 5 $ repeat $ show 0) $ map pure s

  password :: [String] -> String
  password = map (head . drop 5)

  validate :: [String] -> [String]
  validate s = take 8 $ filter isValid s

  generateMD5 :: String -> [String]
  generateMD5 s = map (show . md5 . LBB.toLazyByteString . LBB.stringUtf8 . (++) s . show) plusOne 

  plusOne :: [Integer]
  plusOne = iterate (+1) 0

  -- 00000155f8105dff7f56ee10fa9b9abd
  -- 00000000000000000000000101010101111110000001000001011101111111110111111101010110111011100001000011111010100110111001101010111101

  -- 00000f9a2c309875e05c5a5d09f1b8c4
  -- 00000000000000000000111110011010001011000011000010011000011101011110000001011100010110100101110100001001111100011011100011000100

  -- mask
  -- 00000111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
