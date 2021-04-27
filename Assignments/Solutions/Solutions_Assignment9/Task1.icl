module Task1
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) | Leaf

minRoot :: Tree Int
minRoot = (Node 4 (Node 10 (Node 11 Leaf Leaf)(Node 16 Leaf Leaf)) (Node 22 (Node 15 Leaf Leaf) Leaf))

minMostLeftLeaf :: Tree Int
minMostLeftLeaf = (Node 4 (Node 10 (Node 11 (Node 2 Leaf Leaf) Leaf) Leaf) (Node 16 (Node 22 (Node 13 (Node 15 Leaf Leaf) Leaf) Leaf) (Node 100 Leaf Leaf) ))

minMostRightLeaf :: Tree Int
minMostRightLeaf = (Node 4 (Node 10 (Node 11 (Node 2 Leaf Leaf) Leaf) Leaf) (Node 16 (Node 22 (Node 13 (Node 15 Leaf Leaf) Leaf) Leaf) (Node 100 Leaf (Node 1 Leaf Leaf)) ))

minNode :: Tree Int
minNode = (Node 4 (Node 10 (Node 11 (Node 2 Leaf Leaf) Leaf) Leaf) (Node 16 (Node -12 (Node 13 (Node 15 Leaf Leaf) Leaf) Leaf) (Node 100 Leaf Leaf) ))

/*
1. Given a Tree of integers, find the minimum value of all the nodes.
*/

minTree :: (Tree Int) -> Int


//Start = minTree minRoot // 4
//Start = minTree minMostLeftLeaf //2
//Start = minTree minMostRightLeaf //1
//Start = minTree minNode // -12
