-- Alunos
-- Mateus da Costa e Silva Rios Alves de Andrade - 230806
-- Matheus - 230906

import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)

(|>) :: t1 -> (t1 -> t2) -> t2
x |> f = f x

buildGraph :: (Ord v, Num c) => [(v, v, c)] -> Map v [(v, c)]
buildGraph = foldr insert' Map.empty
  where
    insert' (u, v, c) =
      Map.insertWith (++) u [(v, c)]
        . Map.insertWith (++) v [(u, c)]


-- Versão modificada de dijkstra que apenas calcula o custo de ir de s a t no grafo g
-- representado como uma árvore binária de vértices e suas respectivas listas de adjacências.
dijkstra :: (Ord v, Ord c, Num c) => v -> v -> Map v [(v, c)] -> Maybe c
dijkstra s t g = 
  do
    edges <- Map.lookup s g
    dijkstra' (Map.singleton s 0) Map.empty 0 s t g
  where
    -- Atualizando fronteira, possivelmente reduzindo o custo
    addToVisit cost neighbors toVisit =
        foldr (\(v, c) tv -> Map.insertWith min v (cost + c) tv) toVisit neighbors

    getMin v c Nothing = Just (v, c)
    getMin v c curr@(Just (max_v, max_c)) = if c < max_c then Just (v, c) else curr

    -- visited representa todos os vértices cujo custo mínimo já é conhecido.
    -- toVisit representa a fronteira, os vértices que são atingíveis por um caminho
    -- passando pelos vértices em visited, com o custo mínimo associado.
    -- cost é o custo de se chegar a s desde o vértice original e t o destino final.
    dijkstra' visited toVisit cost s t g
      | s == t    = Just cost
      | otherwise =
        let 
          -- Atualizando a fronteira. Nessa etapa, sabemos que s pertence ao grafo,
          -- portanto Map.lookup não retornará Nothing.
          Just neighbors = Map.lookup s g
          toVisit' = 
            toVisit
              |> Map.delete s
              |> addToVisit cost (filter (\(k, _) -> isNothing $ Map.lookup k visited) neighbors)
        in do
            -- Próximo vértice da busca. Se o foldr retornar Nothing, a execução para aqui.
            (next_v, c) <- Map.foldrWithKey getMin Nothing toVisit'
            -- Se houver vértice na fronteira, chamada recursiva com o estado atualizado.
            dijkstra' (Map.insert next_v c visited) toVisit' c next_v t g

proj4a :: (Num c, Ord v, Ord c) => [(v, v, c)] -> v -> v -> c
proj4a g s t = let Just c = proj4b g s t in c

proj4b :: (Num c, Ord v, Ord c) => [(v, v, c)] -> v -> v -> Maybe c
proj4b g s t = dijkstra s t (buildGraph g)

main :: IO ()
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
