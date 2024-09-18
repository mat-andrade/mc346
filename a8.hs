data Arvore a = Vazia | No a (Arvore a) (Arvore a) deriving (Eq, Show)

menormaior :: (Ord a) => Arvore a -> (a, a)
menormaior (No x Vazia Vazia) = (x, x)
menormaior (No x l Vazia) =
  let (a, b) = menormaior l in (min a x, max b x)
menormaior (No x Vazia r) = menormaior (No x r Vazia)
menormaior (No x l r) = 
  let (minl, maxl) = menormaior l
      (minr, maxr) = menormaior r
  in (min (min minl minr) x,
      max (max maxl maxr) x)
