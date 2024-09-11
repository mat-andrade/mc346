data BinTree a = Leaf | BinTreeNode a (BinTree a) (BinTree a) deriving (Eq, Show, Read)

isEmpty :: BinTree a -> Bool
isEmpty Leaf = True
isEmpty _ = False

value :: BinTree a -> a
value (BinTreeNode a _ _) = a

left :: BinTree a -> BinTree a
left (BinTreeNode _ l _) = l

right :: BinTree a -> BinTree a
right (BinTreeNode _ _ r) = r

maxSearchTree :: (Ord a) => BinTree a -> a
maxSearchTree (BinTreeNode a _ Leaf) = a
maxSearchTree (BinTreeNode a _ r) = maxSearchTree r

minSearchTree :: (Ord a) => BinTree a -> a
minSearchTree (BinTreeNode a Leaf _) = a
minSearchTree (BinTreeNode a l _) = minSearchTree l

isSearchTree :: (Ord a) => BinTree a -> Bool
isSearchTree Leaf = True
isSearchTree (BinTreeNode a Leaf r) = isSearchTree r && a < minSearchTree r
isSearchTree (BinTreeNode a l Leaf) = isSearchTree l && a > maxSearchTree l
isSearchTree (BinTreeNode a l r) =
  isSearchTree l && isSearchTree r && maxSearchTree l < a && a < minSearchTree r

insert :: (Ord a) => a -> BinTree a -> BinTree a
insert x Leaf = BinTreeNode x Leaf Leaf
insert x tree@(BinTreeNode a l r)
  | x < a = BinTreeNode a (insert x l) r
  | x > a = BinTreeNode a l (insert x r)
  | otherwise = tree

remove :: (Ord a) => a -> BinTree a -> BinTree a
remove _ Leaf = Leaf
remove x (BinTreeNode a l r)
  | x < a = BinTreeNode a (remove x l) r
  | x > a = BinTreeNode a l (remove x r)
  | isEmpty r = l
  | isEmpty l = r
  | otherwise = let v = maxSearchTree l in BinTreeNode v (remove v l) r

buildSearchTree :: (Foldable t, Ord a) => t a -> BinTree a
buildSearchTree = foldl (flip insert) Leaf

main :: IO ()
main =
  let
    tree1 = buildSearchTree [6, 1, 4, 9, 9, 2]
    tree2 = insert 0 tree1
    tree3 = insert 10 tree2
    tree4 = insert 8 tree4
    tree5 = remove 6 tree4
   in do
        print tree4
        print tree5
