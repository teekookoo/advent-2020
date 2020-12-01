module Day01
  ( solve1
  , solve2
  ) where

input :: IO [Int]
input = stringToInts <$> readFile "src/inputs/input01"

solve1 :: IO ()
solve1 = do
  nums <- input
  case findProduct . sumsAndProducts $ nums of
    Just p -> putStrLn . show $ p
    Nothing -> putStrLn "No lines sum to 2020"

stringToInts :: String -> [Int]
stringToInts = map read . lines

sumsAndProducts :: Num a => [a] -> [(a, a)]
sumsAndProducts [] = []
sumsAndProducts (x:xs) = map (\y -> (x+y, x*y)) xs ++ (sumsAndProducts xs)

findProduct :: (Eq a, Num a) => [(a, a)] -> Maybe a
findProduct = lookup 2020
  

solve2 :: IO ()
solve2 = do
  nums <- input
  case findProduct (sumsAndProducts2 nums) of
    Just p -> putStrLn (show p)
    Nothing -> putStrLn "No 3 lines sum to 2020"

sumsAndProducts2 :: Num a => [a] -> [(a, a)]
sumsAndProducts2 [] = []
sumsAndProducts2 (x:xs) = zip sums2 products2 ++ sumsAndProducts2 xs
  where (sums, products) = unzip (sumsAndProducts xs)
        sums2 = map (x + ) sums
        products2 = map (x * ) products
