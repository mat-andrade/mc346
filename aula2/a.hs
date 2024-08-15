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

isOrdered :: (Ord a) => [a] -> Bool
isOrdered [] = True
isOrdered [x] = True
isOrdered (a : b : xs) = b >= a && isOrdered xs

shiftr :: [a] -> [a]
shiftr [] = []
shiftr xs =
  let h = head (rev xs)
      t = popLast xs
   in h : t

shiftrn :: (Eq t, Num t) => t -> [a] -> [a]
shiftrn 0 xs = xs
shiftrn n xs = shiftrn (n - 1) (shiftr xs)

shiftl :: [a] -> [a]
shiftl [] = []
shiftl (x : xs) = xs ++ [x]

shiftln :: (Eq t, Num t) => t -> [a] -> [a]
shiftln 0 xs = xs
shiftln n xs = shiftln (n - 1) (shiftl xs)

removeElementOnce :: (Eq a) => a -> [a] -> [a]
removeElementOnce e xs = _removeElementOnce e xs []

_removeElementOnce :: (Eq a) => a -> [a] -> [a] -> [a]
_removeElementOnce _ [] acc = rev acc
_removeElementOnce e (x : xs) acc =
  if e == x
    then
      rev acc ++ xs
    else
      _removeElementOnce e xs (x : acc)

removeElementN :: (Num t1, Eq t1, Eq t2) => t1 -> t2 -> [t2] -> [t2]
removeElementN n e xs = _removeElementN n e xs []

_removeElementN :: (Num t1, Eq t1, Eq t2) => t1 -> t2 -> [t2] -> [t2] -> [t2]
_removeElementN _ _ [] acc = rev acc
_removeElementN 0 _ xs acc = rev acc ++ xs
_removeElementN n e (x : xs) acc =
  if e == x
    then
      _removeElementN (n - 1) e xs acc
    else
      _removeElementN n e xs (x : acc)

removeElementAll :: (Eq a) => a -> [a] -> [a]
removeElementAll e xs = _removeElementAll e xs []

_removeElementAll :: (Eq a) => a -> [a] -> [a] -> [a]
_removeElementAll _ [] acc = rev acc
_removeElementAll e (x : xs) acc =
  if e == x
    then
      _removeElementAll e xs acc
    else
      _removeElementAll e xs (x : acc)

removeElementLast :: (Eq a) => a -> [a] -> [a]
removeElementLast e xs = rev (removeElementOnce e (rev xs))

replaceElementOnce oe ne xs = _replaceElementOnce oe ne xs []
_replaceElementOnce _ _ [] acc = rev acc
_replaceElementOnce oe ne (x : xs) acc =
  if x == oe then
    rev acc ++ (ne : xs)
  else
    _replaceElementOnce oe ne xs (x : acc)

replaceElementAll :: Eq a => a -> a -> [a] -> [a]
replaceElementAll oe ne xs = _replaceElementAll oe ne xs []

_replaceElementAll :: Eq a => a -> a -> [a] -> [a] -> [a]
_replaceElementAll _ _ [] acc = rev acc
_replaceElementAll oe ne (x : xs) acc =
  if x == oe then
    _replaceElementAll oe ne xs (ne : acc)
  else
    _replaceElementAll oe ne xs (x : acc)

replaceElementN :: (Num t, Eq t, Eq a) => t -> a -> a -> [a] -> [a]
replaceElementN n oe ne xs = _replaceElementN n oe ne xs []
_replaceElementN :: (Num t, Eq t, Eq a) => t -> a -> a -> [a] -> [a] -> [a]
_replaceElementN _ _ _ [] acc = rev acc
_replaceElementN 0 _ _ xs acc = rev acc ++ xs
_replaceElementN n oe ne (x : xs) acc =
  if x == oe then
    _replaceElementN (n - 1) oe ne xs (ne : acc)
  else
    _replaceElementN n oe ne xs (x : acc)

main = do
  -- [1,2,3,3,1]
  -- print (_replaceElementAll 1 4 [3, 1] [3, 2, 4])
  print (replaceElementN 3 0 4 [1,2,3,3,1])
