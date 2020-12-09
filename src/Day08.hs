module Day08
  ( solve1
  , solve2
  ) where

import Data.Array.Unboxed
import qualified Data.IntSet as S
import Data.Maybe (listToMaybe, mapMaybe)
import Text.Parsec

data Instruction = Acc Int | Jmp Int | Nop Int | Exit deriving (Show, Eq)
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
    programFile = (++) <$> (instruction `endBy` endOfLine) <*> pure [Exit] <* eof
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
solve2 = do
  prog <- input
  let (ptrMin, ptrMax) = bounds prog
      modify ptr = case prog ! ptr of
        Acc _ -> Nothing
        Jmp x -> Just $ prog // [(ptr, Nop x)]
        Nop x -> Just $ prog // [(ptr, Jmp x)]
        Exit  -> Nothing
      options = mapMaybe modify [ptrMin .. ptrMax]
  case listToMaybe $ mapMaybe (finalState . initialize) options of
    Nothing -> error "No terminating program found"
    Just es  -> print $ accumulator es

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
  Exit  -> es
  where
    ptr = pointer es
    acc = accumulator es

finalState :: ExecutionState -> Maybe ExecutionState
finalState = f S.empty . iterate advance
  where
    f seen (es:ess)
      | program es ! pointer es == Exit = Just es
      | pointer es `S.member` seen      = Nothing
      | otherwise                       = f (S.insert (pointer es) seen) ess
    f _ [] = error "This should never happen"
