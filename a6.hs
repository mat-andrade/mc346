
chave_valor :: Eq k => [(k, v)] -> ([k], [v])
chave_valor kvs = (map2 fst kvs, map2 snd kvs)

map2 _ [] = []
map2 f (x:xs) = f x : map f xs

main = do
  print $ chave_valor [("abc", 1), ("de", 4), ("casa", 2), ("casa2", 2)]