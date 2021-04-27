module Task1
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) | Leaf

tree1 :: Tree Int
tree1 = (Node 4 (Node 10 (Node 6 Leaf Leaf)(Node 11 Leaf Leaf)) (Node 20 (Node 12 Leaf Leaf) Leaf))

tree2 :: Tree Int
tree2 = (Node 5 (Node 10 (Node 31 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 17 (Node 31 (Node 14 (Node 12 Leaf Leaf) Leaf) Leaf) (Node 11 Leaf Leaf) ))

tree3 :: Tree Int
tree3 = (Node 12 (Node 11 (Node 11 (Node 32 Leaf Leaf) Leaf) Leaf) (Node 4 (Node 17 (Node 5 (Node 7 Leaf Leaf) Leaf) Leaf) (Node 3 Leaf (Node 4 Leaf Leaf)) ))

tree4 :: Tree Int
tree4 = (Node 7 (Node 11 tree1 tree2) (Node 5 tree3 tree2))

tree5 :: Tree Int
tree5 = Node 1 tree3 tree4

/* 1. Given the binary tree, count number of leaf nodes in the tree.
*/

countLeaves :: (Tree a) -> Int
countLeaves Leaf = 1
countLeaves (Node x l r) = countLeaves (l) + countLeaves (r) 

//Start = countLeaves tree1 // 7
//Start = countLeaves tree2 // 10
//Start = countLeaves tree3 // 11
//Start = countLeaves tree4 // 38
//Start = countLeaves tree5 // 49
