-- Alunos
-- Mateus da Costa e Silva Rios Alves de Andrade - 230806
-- Matheus - 230906

import Data.Foldable (maximumBy)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Tuple (swap)
import Distribution.Compat.Graph (neighbors)

(|>) :: t1 -> (t1 -> t2) -> t2
x |> f = f x

buildGraph :: (Ord v, Num c) => [(v, v, c)] -> Map v [(v, c)]
buildGraph = foldr insert' Map.empty
  where
    insert' (u, v, c) =
      Map.insertWith (++) u [(v, c)]
        . Map.insertWith (++) v [(u, c)]

dijkstra :: (Ord v, Ord c, Num c) => v -> v -> Map v [(v, c)] -> Maybe c
dijkstra s t g =
  let Just edges = Map.lookup s g
   in dijkstra' (Map.singleton s 0) (Map.fromList edges) 0 s t g
  where
    addToVisit cost neighbors toVisit = foldr (\(v, c) tv -> Map.insert v (cost + c) tv) toVisit neighbors
    getMin v c Nothing = Just (v, c)
    getMin v c curr@(Just (max_v, max_c)) = if c < max_c then Just (v, c) else curr

    dijkstra' visited toVisit cost s t g
      | s == t = Just cost
      | otherwise =
        let 
          Just neighbors = Map.lookup s g
          toVisit' = addToVisit cost (filter (\(k, _) -> isNothing $ Map.lookup k visited) neighbors) toVisit
        in if Map.null toVisit' then Nothing else
          do
            (next_v, c) <- Map.foldrWithKey getMin Nothing toVisit'
            dijkstra' (Map.insert next_v c visited) toVisit' c next_v t g

main = do
  print $
    dijkstra
      "1"
      "6"
      ( buildGraph
          [ ("1", "2", 7),
            ("1", "3", 9),
            ("1", "6", 14),
            ("2", "3", 10),
            ("2", "4", 15),
            ("3", "4", 11),
            ("3", "6", 2),
            ("4", "5", 6),
            ("5", "6", 9)
          ]
      )
