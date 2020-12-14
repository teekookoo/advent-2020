module Day14
  ( solve1
  , solve2
  ) where

import           Control.Applicative (liftA2)
import           Data.Bits           (complementBit, shiftL, (.&.), (.|.))
import           Data.Char           (digitToInt)
import qualified Data.IntMap.Strict  as M
import           Data.List           (foldl')
import           Text.Parsec

type ZeroMask = Int
type OneMask = Int
type FloatMask = [Int]
type Mask = (ZeroMask, OneMask, FloatMask)
type Address = Int
type Value = Int
data Action = SetMask ZeroMask OneMask FloatMask
            | SetMem Address Value
type Program = [Action]

type Memory = M.IntMap Value
data ExecutionState = ES
                    { memory :: Memory
                    , mask   :: Mask }

parseInput :: String -> Program
parseInput s = case parse progP "" s of
  Left err      -> error (show err)
  Right program -> program
  where
    progP = actionP `endBy` endOfLine <* eof
    actionP = try memP <|> maskP
    memP = (\as vs -> SetMem (read as) (read vs)) <$>
      (string "mem[" *> many1 digit <* string "] = ") <*> many1 digit
    maskP = (\ms -> SetMask (zeroMask ms) (oneMask ms) (floatMask ms)) <$>
      (string "mask = " *> many1 (oneOf "01X"))
    zeroMask = readBinary . map (\d -> if d == 'X' then 1 else digitToInt d)
    oneMask  = readBinary . map (\d -> if d == 'X' then 0 else digitToInt d)
    floatMask = map fst . filter ((== 'X') . snd) . zip [0 ..] . reverse
    readBinary = foldl' (\x d -> shiftL x 1 .|. d) (0 :: Int)

solve1 :: IO ()
solve1 = input >>= print . memSum . run
  where
    input = parseInput <$> readFile "src/inputs/input14"
    -- input = pure $ parseInput "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0\n"
    run = foldl' advance initialES
    advance es (SetMask zm om fm) = es { mask = (zm, om, fm) }
    advance es (SetMem addr val) =
      es { memory = M.insert addr (apply (mask es) val) (memory es) }
    apply (zm, om, _) val = (val .&. zm) .|. om
    initialES = ES M.empty (0, -1, [])
    memSum = M.foldl' (+) 0 . memory

solve2 :: IO ()
solve2 = input >>= print . memSum . run
  where
    input = parseInput <$> readFile "src/inputs/input14"
    -- input = pure $ parseInput "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1\n"
    run = foldl' advance $ ES M.empty (0, -1, [])
    advance es (SetMask zm om fm) = es { mask = (zm, om, fm) }
    advance es (SetMem addr val) =
      es { memory = insertAll val (memory es) (apply (mask es) addr) }
    insertAll v = foldl' (\mem addr -> M.insert addr v mem)
    apply (_, om, fm) a = applyFloats fm (a .|. om)
    applyFloats fm a = map (\f -> f a) . foldl' (liftA2 (.)) [id] $
      map (\i -> [id, (`complementBit` i)]) fm
    memSum = M.foldl' (+) 0 . memory
