module Day16
  ( solve1
  , solve2
  ) where

import           Data.List   (foldl', isPrefixOf, transpose)
import           Data.Maybe  (mapMaybe)
import Data.Set (Set, (\\))
import qualified Data.Set    as S
import           Text.Parsec

type Range = (Int, Int)
type Ticket = [Int]

data Field = Field String Range Range deriving Show

input :: IO ([Field], Ticket, [Ticket])
input = unpack . parse ticketFile "" <$> readFile "src/inputs/input16"
  where
    unpack (Left err) = error $ show err
    unpack (Right x)  = x
    ticketFile = (,,) <$> (fields <* endOfLine)
                      <*> (ownTicket <* endOfLine)
                      <*> (otherTickets <* eof)
    fields = field `endBy1` endOfLine
    field = Field <$> (many1 (lower <|> char ' ') <* string ": ")
                  <*> (range <* string " or ")
                  <*> range
    range = (\lo hi -> (read lo, read hi)) <$> (many1 digit <* char '-')
                                           <*> many1 digit
    ownTicket = string "your ticket:" *> endOfLine *> nums <* endOfLine
    otherTickets =
      string "nearby tickets:" *> endOfLine *> (nums `endBy1` endOfLine)
    nums = map read <$> (many1 digit `sepBy1` char ',')

testInput :: IO ([Field], Ticket, [Ticket])
testInput = pure ( [ Field "class" (1, 3) (5, 7)
                   , Field "row" (6, 11) (33, 44)
                   , Field "seat" (13, 40) (45, 50)]
                 , [7,1,14]
                 , [[7,3,47],[40,4,50],[55,2,20],[38,6,12]])

testInput2 :: IO ([Field], Ticket, [Ticket])
testInput2 = pure ( [ Field "class" (0, 1) (4, 19)
                    , Field "row" (0, 5) (8, 19)
                    , Field "seat" (0, 13) (16, 19) ]
                  , [11, 12, 13]
                  , [[3,9,18], [15,1,5], [5,14,9]]
                  )

solve1 :: IO ()
solve1 = input >>= print . foldl' (+) 0 . invalidValues
-- solve1 = testInput >>= print . foldl' (+) 0 . invalidValues
  where
    invalidValues (fields, _, tickets) =
      concatMap (filter (invalid fields)) tickets
    invalid fs n = all (not . (`allows` n)) fs

solve2 :: IO ()
solve2 = input >>= print . result
-- solve2 = testInput2 >>= mapM_ print . result
  where
    result (fs, ot, ts) = foldl' (*) 1 . pick "departure" . (`zip` ot) .
                          identify fs $ mapMaybe (validate fs) ts
    pick s = map snd . filter (isPrefixOf s . fst)
    identify fs = resolve . map (S.fromList . possibleFields fs) . transpose
    possibleFields fs ns = map (\(Field name _ _) -> name) $
                           filter (\f -> all (f `allows`) ns) fs

validate :: [Field] -> Ticket -> Maybe Ticket
validate fs = mapM validate'
  where validate' n | any (`allows` n) fs = Just n
                    | otherwise = Nothing

allows :: Field -> Int -> Bool
(Field _ (a, b) (c, d)) `allows` n = a <= n && n <= b || c <= n && n <= d

-- Not particularly efficient
-- Also bad variable naming
resolve :: [Set String] -> [String]
resolve = resolve' S.empty
  where
    resolve' resolved sss
      | S.size resolved == length sss = map (S.elemAt 0) sss
      | otherwise = uncurry resolve' $ foldr f (resolved, []) sss
    f ss (resolved, sss)
      | S.size ss == 1 = (S.insert (S.elemAt 0 ss) resolved, ss:sss)
      | otherwise = (resolved, (ss \\ resolved) : sss)
