trocatodos :: Eq a => a -> a -> [a] -> [a]
trocatodos _ _ [] = []
trocatodos oe ne (x : xs)
  | x == oe = ne : r
  | otherwise = x : r
  where r = trocatodos oe ne xs

cumsum :: Num t => [t] -> [t]
cumsum xs = cumsum' xs 0
  where
    cumsum' [] _ = []
    cumsum' (x : xs) s = (s + x) : cumsum' xs (s + x)

main :: IO ()
main = do
  print (cumsum [5,10,2,3])
