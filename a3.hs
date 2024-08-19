
posicoes :: (Eq t, Num a) => t -> [t] -> [a]
posicoes it xs = posicoes' it xs 1 []
  where
    posicoes' _ [] _ acc = reverse acc
    posicoes' it (x : xs) n acc
      | x == it = posicoes' it xs (n + 1) (n : acc)
      | otherwise = posicoes' it xs (n + 1) acc

split :: Eq a => [a] -> a -> [[a]]
split xs delim = split' xs delim []
  where
    split' [] _ acc = [reverse acc]
    split' (x : xs) delim acc
      | x == delim = [reverse acc, xs]
      | otherwise = split' xs delim (x : acc)

splitall :: Eq a => [a] -> a -> [[a]]
splitall xs delim = splitall' xs delim [[]]
  where
    splitall' [] _ (a : acc) = reverse (reverse a : acc)
    splitall' (x : xs) delim (a : acc)
      | x == delim = splitall' xs delim ([] : reverse a : acc)
      | otherwise = splitall' xs delim ((x : a) : acc)

drop2 :: (Eq t, Num t) => t -> [a] -> [a]
drop2 _ [] = []
drop2 0 xs = xs
drop2 n (x : xs) = drop2 (n - 1) xs

take2 :: (Eq t, Num t) => t -> [a] -> [a]
take2 _ [] = []
take2 0 _ = []
take2 n (x : xs) = x : take2 (n - 1) xs

main :: IO ()
main = print (take2 100 "qwertyuiopoiuytxxt")
