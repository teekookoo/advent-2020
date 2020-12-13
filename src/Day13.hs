module Day13
  ( solve1
  , solve2
  ) where

import           Data.Text      (Text)
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Data.Text.Read as R

parse1 :: Text -> (Int, [Int])
parse1 inputText = (r earliest, map r buses)
  where
    r t = case R.decimal t of
      Right (n, _) -> n
      Left _       -> error "Should be int"
    [earliest, busesStr] = T.lines inputText
    buses = filter (/= T.pack "x") $ T.splitOn (T.pack ",") busesStr

parse2 :: Text -> [(Int, Int)]
parse2 = map secondToInt . filter notX . zip [0..] . split . secondLine
  where
    secondToInt (i, t) = case R.decimal t of
      Right (n, _) -> (i, n)
      Left _       -> error "Should be int"
    notX = (/= T.pack "x") . snd
    split = T.splitOn (T.pack ",")
    secondLine = (!! 1) . T.lines

solve1 :: IO ()
solve1 = do
  (start, buses) <- input
  let (d, b) = minimum $ departures start buses
  print ((d - start) * b)
  where
    input = parse1 <$> T.readFile "src/inputs/input13"
    -- input = pure (939, [7,13,59,31,19]) :: IO (Int, [Int])
    earliestDeparture s b = b * ((s - 1) `div` b + 1)
    departures s b = map (earliestDeparture s) b `zip` b

solve2 :: IO ()
solve2 = do
  buses <- input
  print (align buses)
  where
    input = parse2 <$> T.readFile "src/inputs/input13"
    -- input = pure $ [0, 1, 4, 6, 7] `zip` [7,13,59,31,19] :: IO [(Int, Int)]
    align ((diff, step) : buses) = align' diff step buses
    align []                     = error "No buses given"
    align' t _ [] = t
    align' t dt bs@((diff, step) : rest)
      | validTime t diff step = align' t (lcm dt step) rest
      | otherwise = align' (t + dt) dt bs
    validTime t d s = (t + d) `mod` s == 0
