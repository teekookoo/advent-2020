module Day19
  ( solve1
  , solve2
  ) where

import qualified Data.IntMap.Lazy as M
import           Data.List        (foldl1')
import           Data.Maybe       (mapMaybe)
import           Text.Parsec

type Message = String

data RawRule = Character' Char
             | Or' RawRule RawRule
             | Seq' RawRule RawRule
             | Ref Int

data Rule = Character Char
          | Or Rule Rule
          | Seq Rule Rule
          deriving (Show)

data ParseState = Parsing String String

solve1 :: IO ()
solve1 = do
  -- (rule, messages) <- testInput
  (rule, messages) <- input
  print . length $ mapMaybe (validate rule) messages

solve2 :: IO ()
solve2 = do
  -- (rule, messages) <- testInput2
  (rule, messages) <- input2
  print . length $ mapMaybe (validate rule) messages

input :: IO (Rule, [Message])
input = parseInput <$> readFile "src/inputs/input19_1"

input2 :: IO (Rule, [Message])
input2 = parseInput <$> readFile "src/inputs/input19_2"

testInput :: IO (Rule, [Message])
testInput = pure $ parseInput "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\n\nababbb\nbababa\nabbbab\naaabbb\naaaabbb\n"

testInput2 :: IO (Rule, [Message])
testInput2 = parseInput <$> readFile "src/inputs/input19_2_test"

parseInput :: String -> (Rule, [Message])
parseInput s = case parse puzzleInput "" s of
  Left err  -> error $ show err
  Right res -> res
  where
    puzzleInput =
      (,) <$> (parseRule <$> rules) <*> (endOfLine *> messages) <* eof
    rules = rule `endBy1` endOfLine
    rule = (,) <$> (read <$> (many1 digit <* string ": ")) <*> rawRule
    rawRule = try (Character' <$> (char '"' *> oneOf "ab" <* char '"')) <|>
              foldl1' Or' <$> (integers `sepBy1` string "| ")
    integers = foldl1' Seq' <$>
               many1 (Ref . read <$> many1 digit <* skipMany (char ' '))
    messages = many1 (oneOf "ab") `endBy1` endOfLine

parseRule :: [(Int, RawRule)] -> Rule
parseRule rawRules = rules M.! 0
  where
    rules = M.fromList [(i, toRule rr) | (i, rr) <- rawRules]
    toRule (Character' c) = Character c
    toRule (Or' r1 r2)    = toRule r1 `Or` toRule r2
    toRule (Seq' r1 r2)   = toRule r1 `Seq` toRule r2
    toRule (Ref i)        = rules M.! i

validate :: Rule -> Message -> Maybe Message
validate rule message =
  foldr takeSuccessful Nothing (apply rule (Parsing "" message))
  where
    takeSuccessful (Parsing gsm "") _ = Just $ reverse gsm
    takeSuccessful _ next             = next

apply :: Rule -> ParseState -> [ParseState]
apply (Character _) (Parsing _ "") = []
apply (Character c) (Parsing parsed (x:xs))
  | c == x = [Parsing (x:parsed) xs]
  | otherwise = []
apply (Or r1 r2) ps = apply r1 ps ++ apply r2 ps
apply (Seq r1 r2) ps = concatMap (apply r2) (apply r1 ps)
