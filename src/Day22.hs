{-# LANGUAGE BangPatterns #-}

module Day22
  ( solve1
  , solve2
  ) where

import           Data.Monoid   (Sum (..))
import           Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Text.Parsec   hiding (Empty)

type Deck = Seq Int
type History = Set (Deck, Deck)
data Status = Completed Result | Playing Deck Deck
data Status2 = Completed2 Result | Playing2 History Deck Deck
data Result = Win1 Deck | Win2 Deck

solve1 :: IO ()
solve1 = do
  result <- uncurry playGame <$> input
  -- result <- uncurry playGame <$> testInput
  case result of
    Win1 d -> putStrLn $ "Player 1: " ++ show (score d)
    Win2 d -> putStrLn $ "Player 2: " ++ show (score d)

solve2 :: IO ()
solve2 = do
  result <- uncurry playGame2 <$> input
  -- result <- uncurry playGame2 <$> testInput
  case result of
    Win1 d -> putStrLn $ "Player 1: " ++ show (score d)
    Win2 d -> putStrLn $ "Player 2: " ++ show (score d)

-- Handle input

parseInput :: String -> (Deck, Deck)
parseInput s = case parse decks "" s of
  Left err  -> error $ show err
  Right res -> res
  where
    decks = (,) <$> (deck <* endOfLine) <*> (deck <* eof)
    -- deck = skipMany (letter <|> oneOf " :" <|> endOfLine) *>
    deck = string "Player " *> digit *> char ':' *> endOfLine *>
           (Seq.fromList . map read <$> many1 digit `endBy1` endOfLine)

input :: IO (Deck, Deck)
input = parseInput <$> readFile "src/inputs/input22"

testInput :: IO (Deck, Deck)
testInput = parseInput <$> readFile "src/inputs/input22_test"

-- Business logic

score :: Deck -> Int
score d = getSum $ Seq.foldMapWithIndex (\i c -> Sum $ (Seq.length d - i) * c) d

playGame :: Deck -> Deck -> Result
playGame d1 d2 = case playRound d1 d2 of
  Completed res   -> res
  Playing d1' d2' -> playGame d1' d2'

playRound :: Deck -> Deck -> Status
playRound d Empty = Completed $ Win1 d
playRound Empty d = Completed $ Win2 d
playRound (c1 :<| d1) (c2 :<| d2)
  | c1 > c2 = Playing (d1 :|> c1 :|> c2) d2
  | otherwise = Playing d1 (d2 :|> c2 :|> c1)

playGame2 :: Deck -> Deck -> Result
playGame2 = go Set.empty
  where
    go history d1 d2 = case playRound2 history d1 d2 of
      Completed2 res            -> res
      Playing2 history' d1' d2' -> go history' d1' d2'

playRound2 :: History -> Deck -> Deck -> Status2
playRound2 _ d Empty = Completed2 $ Win1 d
playRound2 _ Empty d = Completed2 $ Win2 d
playRound2 h d1 d2
  | (d1, d2) `Set.member` h = Completed2 $ Win1 d1
playRound2 history (c1 :<| d1) (c2 :<| d2)
  | canRecurse = case playGame2 (Seq.take c1 d1) (Seq.take c2 d2) of
      Win1 _ -> p1Won
      Win2 _ -> p2Won
  | c1 > c2 = p1Won
  | otherwise = p2Won
  where
    canRecurse = c1 <= Seq.length d1 && c2 <= Seq.length d2
    p1Won = Playing2 history' (d1 :|> c1 :|> c2) d2
    p2Won = Playing2 history' d1 (d2 :|> c2 :|> c1)
    history' = Set.insert (c1 :<| d1, c2 :<| d2) history

