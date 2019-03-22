module BinTree where

data BinaryTree a =
                  Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node lhs v rhs)
    | b == v    = Node lhs v rhs
    | b  < v    = Node (insert' b lhs) v rhs
    | otherwise = Node lhs v (insert' b rhs)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node lhs v rhs) = Node (mapTree f lhs) (f v) (mapTree f rhs)

testTree :: BinaryTree Integer
testTree = Node
           (Node Leaf 3 Leaf)
           4
           (Node Leaf 5 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node lhs v rhs) = v : (preorder lhs ++ preorder rhs)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node lhs v rhs) = (inorder lhs) ++ [v] ++ (inorder rhs)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node lhs v rhs) = (postorder lhs) ++ (postorder rhs) ++ [v]


foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ accum Leaf = accum
foldTree f accum (Node lhs v rhs) =
    -- Post order
    let accumL = (foldTree f accum  lhs)
        accumR = (foldTree f accumL rhs)
    in f v accumR


foldTree' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree' _ accum Leaf = accum
foldTree' f accum (Node lhs v rhs) =
    -- Pre order
    let accum' = f v accum
        accumL = (foldTree f accum' lhs)
    in foldTree f accumL rhs


foldTree'' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree'' _ accum Leaf = accum
foldTree'' f accum (Node lhs v rhs) =
    -- In order
    let accumL = (foldTree f accum lhs)
        accum' = f v accumL
    in foldTree f accum' rhs
