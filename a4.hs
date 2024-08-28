(|>) :: t1 -> (t1 -> t2) -> t2
x |> f = f x

vecprod :: (Num t) => [t] -> [t] -> t
vecprod u v = sum $ zipWith (*) u v

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose ls =
  let (col, resto) = udc ls
   in col : transpose resto
  where
    udc ls = (map head ls, map tail ls)

matmul :: (Num c) => [[c]] -> [[c]] -> [[c]]
matmul m1 m2 = matmul' m1 (transpose m2)
  where
    matmul' [] _ = []
    matmul' (l : ls) cs = map (vecprod l) cs : matmul' ls cs

main :: IO ()
main =
  let matriz = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
   in do
        print matriz
        print $ transpose matriz
        print (matmul matriz matriz)
