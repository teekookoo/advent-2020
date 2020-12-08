module Day08
  ( solve1
  , solve2
  ) where

import Data.Array.Unboxed
import qualified Data.IntSet as S
import Text.Parsec

data Instruction = Acc Int | Jmp Int | Nop Int deriving Show
type Program = Array Int Instruction
data ExecutionState = ExecutionState { program :: Program
                                     , pointer :: Int
                                     , accumulator :: Int
                                     }

input :: IO Program
input = do
  contents <- readFile "src/inputs/input08"
  case parse programFile "" contents of
    Left err -> print err >> return (mkArray [])
    Right res -> return $ mkArray res
  where
    mkArray xs = listArray (1, length xs) xs
    programFile = instruction `endBy` endOfLine <* eof
    instruction = choice $ map try [acc, jmp, nop]
    acc = Acc <$> (string "acc " *> int)
    jmp = Jmp <$> (string "jmp " *> int)
    nop = Nop <$> (string "nop " *> int)
    int = read <$> intPrefix <> many1 digit
    intPrefix = char '+' *> pure "" <|> string "-"

solve1 :: IO ()
solve1 = do
  prog <- input
  let states = iterate advance $ initialize prog
      seenPtrs = scanl (\ptrs es -> S.insert (pointer es) ptrs) S.empty states
      notSeen = \(es, ptrs) -> S.notMember (pointer es) ptrs
      extract = fst . last
      lastBefore = extract . takeWhile notSeen $ states `zip` seenPtrs
  print $ accumulator lastBefore
    

solve2 :: IO ()
solve2 = print "Not implemented"

initialize :: Program -> ExecutionState
initialize prog = ExecutionState { program = prog
                                 , pointer = fst $ bounds prog
                                 , accumulator = 0
                                 }

advance :: ExecutionState -> ExecutionState
advance es = case program es ! ptr of
  Acc x -> es { pointer = ptr + 1, accumulator = acc + x }
  Jmp i -> es { pointer = ptr + i }
  Nop _ -> es { pointer = ptr + 1 }
  where
    ptr = pointer es
    acc = accumulator es
