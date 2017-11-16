module Day4 (resulta, resultb, parseInput) where
  
  import qualified Text.Parsec as Parsec

  type Sector = Int
  type Checksum = String
  type Name = String

  data Room = Room Name Sector Checksum 

  resulta :: String -> Int
  resulta str = 0

  resultb :: String -> Int
  resultb str = 0

  parse :: String -> Either Parsec.ParseError [Char]
  parse str =
    Parsec.parse (Parsec.many (Parsec.char 'h')) "" "hhhheeelllooo!"

  parseInput :: String -> (String, Int, String)
  parseInput str = ("",0,"")

  -- totally-real-room-200[decoy]
  -- a-b-c-d-e-f-g-h-987[abcde]
