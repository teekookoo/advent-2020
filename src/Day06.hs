module Day06
  ( solve1
  , solve2
  ) where

import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec

input :: IO [[Set Char]]
input = do
  contents <- readFile "src/inputs/input06"
  let answerLists = parse answers "" contents
  case answerLists of
    Left err -> print err >> return []
    Right l -> return $ map (map S.fromList) l
  where
    groupAnswers = endBy1 (many1 letter) endOfLine
    answers = sepBy groupAnswers endOfLine <* eof

solve1 :: IO ()
solve1 = input >>= print . sum . map (S.size . S.unions)

solve2 :: IO ()
solve2 = input >>= print . sum . map (S.size . intersections)
  where
    intersections (s:ss) = foldl S.intersection s ss
    intersections [] = S.empty
