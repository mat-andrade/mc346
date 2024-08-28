comprime :: (Eq a, Num b) => [a] -> [(a, b)]
comprime (x : xs) = comprime' xs (x, 1)
  where
    comprime' [] a = [a]
    comprime' (x : xs) (a, n)
      | x == a = comprime' xs (a, n + 1)
      | otherwise = (a, n) : comprime' xs (x, 1)

descomprime :: (Eq a) => [(a, Int)] -> [a]
descomprime = concatMap du
  where
    du (_, 0) = []
    du (x, n) = x : du (x, n - 1)

main = do
  print $ comprime "aaabbaasxbbbb"
  print $ descomprime $ comprime "aaabbaasxbbbb"
