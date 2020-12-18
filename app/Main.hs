module Main where

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
-- import qualified Day19
-- import qualified Day20
-- import qualified Day21
-- import qualified Day22
-- import qualified Day23
-- import qualified Day24
-- import qualified Day25
import Options.Applicative

data Options = ShowOne Int
             | ShowAll

main :: IO ()
main = do
  opts <- execParser options
  let datedSolvers = [1..] `zip` solvers
  case opts of
    ShowOne day -> uncurry decorateDay (datedSolvers !! (day - 1))
    ShowAll -> mapM_ (uncurry decorateDay) datedSolvers

decorateDay :: Int -> (IO (), IO ()) -> IO ()
decorateDay d (p1, p2) =
  putStrLn ("Day " ++ show d) >>
  putStr "  Part 1: " >> p1 >>
  putStr "  Part 2: " >> p2

options :: ParserInfo Options
options = info (optParser <**> helper) infoContents
  where
    optParser = parseOne <|> parseAll
    parseOne = ShowOne <$> dayOpt
    dayOpt = option auto
             ( long "day"
               <> short 'd'
               <> value latest
               <> metavar "DAY"
               <> help ("Show solution for day DAY (between 1 and 25). "
                       ++ "Defaults to latest available day")
             )
    latest = length solvers
    parseAll = flag' ShowAll (
      long "all"
        <> short 'a'
        <> help "Show solutions for all solved problems"
      )
    infoContents = fullDesc
                   <> progDesc ("Show solutions to Advent of code 2020 problems. "
                                ++ "Defaults to latest implemented day")
                   <> header "advent2020 - Advent of Code 2020"


solvers :: [(IO (), IO ())]
solvers = [ (Day01.solve1, Day02.solve2)
          , (Day02.solve1, Day02.solve2)
          , (Day03.solve1, Day03.solve2)
          , (Day04.solve1, Day04.solve2)
          , (Day05.solve1, Day05.solve2)
          , (Day06.solve1, Day06.solve2)
          , (Day07.solve1, Day07.solve2)
          , (Day08.solve1, Day08.solve2)
          , (Day09.solve1, Day09.solve2)
          , (Day10.solve1, Day10.solve2)
          , (Day11.solve1, Day11.solve2)
          , (Day12.solve1, Day12.solve2)
          , (Day13.solve1, Day13.solve2)
          , (Day14.solve1, Day14.solve2)
          , (Day15.solve1, Day15.solve2)
          , (Day16.solve1, Day16.solve2)
          , (Day17.solve1, Day17.solve2)
          , (Day18.solve1, Day18.solve2)
          -- , (Day19.solve1, Day19.solve2)
          -- , (Day20.solve1, Day20.solve2)
          -- , (Day21.solve1, Day21.solve2)
          -- , (Day22.solve1, Day22.solve2)
          -- , (Day23.solve1, Day23.solve2)
          -- , (Day24.solve1, Day24.solve2)
          -- , (Day25.solve1, Day25.solve2)
          ]
