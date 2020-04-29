module Day17 (resulta) where

  import qualified Data.ByteString.Char8 as C
  import Crypto.Hash
  import Data.Maybe

  data Direction = U | D | L | R | Closed deriving (Eq, Show)

  -- find the md5 representation
  md5 :: Maybe ((Int,Int), String) -> String
  md5 Nothing = ""
  md5 (Just (_, s)) = (show . (hash :: (C.ByteString -> Digest MD5)) . C.pack) s

  -- check if the door is open
  isOpen :: Char -> Direction -> Direction
  isOpen c d
    | c `elem` "bcdef" = d
    | otherwise = Closed

  -- find the open doors
  scout :: String -> [Direction]
  scout s = zipWith isOpen doors [U,D,L,R]
    where doors = take 4 s

  -- append direction to the md5
  append :: String -> Direction -> String
  append s (U) = s ++ "U"
  append s (D) = s ++ "D"
  append s (L) = s ++ "L"
  append s (R) = s ++ "R"

  -- is the path possible
  traversable :: String -> (Int,Int) -> Direction -> Maybe ((Int,Int), String)
  traversable s (x,y) d
    | x < 0 || y < 0 || x > 3 || y > 3 = Nothing
    | d == Closed = Nothing
    | otherwise = Just $ ((x,y), append s d)

  -- get all valid neighbor rooms
  neighbors :: Maybe ((Int,Int), String) -> [Direction] -> [Maybe ((Int,Int), String)]
  neighbors Nothing _ = pure Nothing
  neighbors (Just ((x,y), s)) ds = filter isJust $ zipWith (traversable s) coords ds
    where coords = map (\(x',y') -> (x+x', y+y')) [(0,(-1)),(0,1),((-1),0),(1,0)]

  -- have we reached the end room
  done :: Maybe ((Int,Int), String) -> Bool
  done (Just ((x,y), _)) = x == 3 && y == 3
  done (Nothing) = False

  -- extract the final result
  extract :: Maybe ((Int,Int), String) -> String
  extract (Just (_, s)) = drop 8 s

  resulta :: [Maybe ((Int,Int),String)] -> String
  resulta as = if any done as then extract $ head $ filter done as else resulta ((tail as) ++ next)
    where a = head as
          next = (neighbors a) $ scout m5
          m5 = md5 a
