import Data.Char (isLetter, toLower)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

(|>) :: t1 -> (t1 -> t2) -> t2
x |> f = f x

incrementar :: (Ord k, Num a) => Map k a -> k -> Map k a
incrementar conta a = Map.insertWith (+) a 1 conta

letra_mais_comum :: [Char] -> Char
letra_mais_comum str =
  str
    |> filter isLetter
    |> map toLower
    |> foldl incrementar Map.empty
    |> Map.foldrWithKey
      ( \k c (max, max_count) ->
          if c > max_count
            then (k, c)
            else (max, max_count)
      )
      ('\0', 0)
    |> fst

main :: IO ()
main = do
  print $ letra_mais_comum "77,88 a!? abc BB 8 8    8  fyt"
