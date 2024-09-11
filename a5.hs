mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort xs = mergesort' xs (length xs)
  where
    mergesort' _ 0 = []
    mergesort' xs 1 = xs
    mergesort' xs n =
      let n' = n `div` 2
          (l, r) = split xs n' []
       in merge (mergesort' l (if even n then n' else n' + 1)) (mergesort' r n')
      where
        split xs 0 acc = (acc, xs)
        split (x : xs) n acc = split xs (n - 1) (x : acc)

        merge [] rs = rs
        merge ls [] = ls
        merge (l : ls) (r : rs)
          | l <= r = l : merge ls (r : rs)
          | otherwise = r : merge (l : ls) rs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort xs =
  let (l, r) = partition xs
   in quicksort l ++ quicksort r
  where
    partition xs = (xs, xs)

main :: IO ()
main = do
  print $ mergesort [1, 3, 5, 2, 0, -4, 7, -8]
