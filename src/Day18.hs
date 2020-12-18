module Day18
  ( solve1
  , solve2
  ) where

import           Data.List   (foldl')
import           Text.Parsec

input :: IO [String]
input = lines <$> readFile "src/inputs/input18"

testInput :: IO [String]
testInput = pure [ "1 + 2 * 3 + 4 * 5 + 6"
                 , "2 * 3 + (4 * 5)"
                 , "5 + (8 * 3 + 9 + 3 * 4 * 3)"
                 , "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
                 , "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
                 ]

solve1 :: IO ()
solve1 = input >>= print . foldl' (+) 0 . map parseExpression
-- solve1 = testInput >>= mapM_ (print . evaluate . parseExpression)

solve2 :: IO ()
solve2 = input >>= print . foldl' (+) 0 . map parseExpression2
-- solve2 = testInput >>= mapM_ (print . evaluate . parseExpression2)

parseExpression :: String -> Int
parseExpression s = case parse (expression <* eof) "" s of
  Left err  -> error $ show err
  Right res -> res
  where
    expression = term `chainl1` addOrMultiply
    addOrMultiply = try ((+) <$ char '+') <|> ((*) <$ char '*')
    term = skipSpace *> (bracketed expression <|> constant) <* skipSpace

parseExpression2 :: String -> Int
parseExpression2 s = case parse (expression <* eof) "" s of
  Left err  -> error $ show err
  Right res -> res
  where
    expression = factor `chainl1` multiply
    factor = term `chainl1` add
    term = skipSpace *> (bracketed expression <|> constant) <* skipSpace
    add = (+) <$ char '+'
    multiply = (*) <$ char '*'

constant :: Parsec String () Int
constant = read <$> many1 digit

skipSpace :: Parsec String () ()
skipSpace = skipMany (char ' ')

bracketed :: Parsec String () Int -> Parsec String () Int
bracketed = between (char '(') (char ')')
