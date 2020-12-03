module Day02
  ( solve1
  , solve2
  ) where

data Policy = Policy Int Int Char deriving Show
type Password = String
data Record = Record Policy Password deriving Show

solve1 :: IO ()
solve1 = do
  records <- input
  putStrLn . show . length $ filter isValid records

solve2 :: IO ()
solve2 = input >>= putStrLn . show . length . filter isValid2

input :: IO [Record]
input = readFile "src/inputs/input02" >>= return . map parseRecord . lines

parseRecord :: String -> Record
parseRecord recStr = Record (Policy (read a) (read b) (head char)) pass
  where
    [range, char, pass] = words $ filter (/= ':') recStr
    [a, b] = foldr f [""] range
    f = (\c (x:xs) -> if c == '-' then ["", x] else (c:x):xs)

isValid :: Record -> Bool
isValid (Record (Policy a b c) password)
        | occurrences < a = False
        | occurrences > b = False
        | otherwise = True
  where occurrences = length $ filter (== c) password

isValid2 :: Record -> Bool
isValid2 (Record (Policy a b c) password) =
  matches a `xor` matches b
  where
    xor x y = (x && not y) || (not x && y)
    matches idx = password !! (idx - 1) == c
        
