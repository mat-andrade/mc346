main :: IO ()
tamanho :: (Num t) => [a] -> t
tamanho xs = tamanhoRec xs 0

tamanhoRec :: (Num t) => [a] -> t -> t
tamanhoRec [] acc = acc
tamanhoRec (x : xs) acc = tamanhoRec xs (acc + 1)

count :: (Eq t1, Num t2) => t1 -> [t1] -> t2
count e xs = countRec e xs 0

countRec :: (Eq t1, Num t2) => t1 -> [t1] -> t2 -> t2
countRec _ [] acc = acc
countRec e (x : xs) acc =
  if e == x
    then countRec e xs (acc + 1)
    else countRec e xs acc

sum2 :: (Num t) => [t] -> t
sum2 xs = _sum xs 0

_sum :: (Num t) => [t] -> t -> t
_sum [] acc = acc
_sum (x : xs) acc = _sum xs (acc + x)

sumEven :: (Integral t) => [t] -> t
sumEven xs = _sumEven xs 0

_sumEven :: (Integral t) => [t] -> t -> t
_sumEven [] acc = acc
_sumEven (x : xs) acc =
  if even x
    then _sumEven xs (acc + x)
    else _sumEven xs acc

pertence :: (Eq t) => t -> [t] -> Bool
pertence _ [] = False
pertence e (x : xs) = (e == x) || pertence e xs

rangeRev :: (Eq a, Num a) => a -> [a]
rangeRev n = reverse (_rangeRev n [])

_rangeRev :: (Eq a, Num a) => a -> [a] -> [a]
_rangeRev 0 acc = acc
_rangeRev n acc = _rangeRev (n - 1) (n : acc)

indexOf :: (Num t1, Eq t2) => t2 -> [t2] -> t1
indexOf e xs = _indexOf e xs 1

_indexOf :: (Num t1, Eq t2) => t2 -> [t2] -> t1 -> t1
_indexOf _ [] _ = 0
_indexOf e (x : xs) acc =
  if e == x
    then acc
    else _indexOf e xs (acc + 1)

rev :: [a] -> [a]
rev xs = _rev xs []

_rev :: [a] -> [a] -> [a]
_rev [] acc = acc
_rev (x : xs) acc = _rev xs (x : acc)

range2 :: (Eq a, Num a) => a -> [a]
range2 n = _range2 n []
_range2 :: (Eq a, Num a) => a -> [a] -> [a]
_range2 0 acc = acc
_range2 n acc = _range2 (n - 1) (n : acc)

popLast :: [a] -> [a]
popLast xs = _popLast xs []
_popLast :: [a] -> [a] -> [a]
_popLast [] acc = rev acc
_popLast [x] acc = rev acc
_popLast (x : xs) acc = _popLast xs (x : acc)

intercala1 :: [a] -> [a] -> [a]
intercala1 [] _ = []
intercala1 _ [] = []
intercala1 (x : xs) (y : ys) = x : y : intercala1 xs ys

intercala2 :: [a] -> [a] -> [a]
intercala2 xs ys = _intercala2 xs ys []

_intercala2 :: [a] -> [a] -> [a] -> [a]
_intercala2 [] ys acc = acc ++ ys
_intercala2 xs [] acc = acc ++ xs
_intercala2 (x : xs) (y : ys) acc = _intercala2 xs ys (acc ++ [x, y])

main = do
  print (intercala1 [1,2,3] [4,5,6,7,8])
