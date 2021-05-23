
/*
This is the standard definition of Binary Tree.
The Tree can be of a given type 'a'.
At each Node, it will contain a key value of type 'a',
and two subtrees, which are Trees of the same type 'a'.
*/
:: Tree a = Node a (Tree a) (Tree a) | Leaf
//:: RoseTree a = Node a [(RoseTree a)] | Leaf
//Below are some convenient trees to work with for
//exercises and testing.
ourTree :: (Tree Int)
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))
myTree Leaf = []
myTree (Node x l r) = (myTree l) ++ [x] ++ (myTree r)
myTree1 t = sum(myTree t)
//Start = myTree1  messyTree


//Start = ourTree

messyTree :: (Tree Int)
messyTree = Node 5(Node 12(Node 8 Leaf (Node 1 Leaf Leaf))(Node 6 (Node 9 Leaf Leaf) Leaf))(Node 2 (Node 3 Leaf (Node 13(Node 100 Leaf Leaf)(Node 21 Leaf Leaf)))(Node 40 (Node 60 (Node 70 (ourTree) Leaf) Leaf) Leaf))
//Start = messyTree

emptyTree :: (Tree Int)
emptyTree = Leaf

singleTree :: (Tree Int)
singleTree = Node 5 Leaf Leaf


//dateTree :: (Tree Date)
//dateTree = Node {year = 2000, day = 4, month=3} (Node {year=1999,month=5,day=1} Leaf Leaf) Leaf //(Node (Date 2000 4 3) (Node (Date 1999 1 5) Leaf Leaf) Leaf)

specialTree :: (Tree Int)
specialTree = Node 30 (Node 15 (Node 7 Leaf Leaf)(Node 22 (Node 17 Leaf Leaf)(Node 27 Leaf Leaf)))(Node 60 (Node 45 Leaf Leaf)(Node 75 Leaf Leaf))

//Start = dateTree

//Functions for a Binary Search Tree

//Getting the value at the node
getNode :: (Tree a) -> a
getNode (Node x l r) = x
//Start = getNode ourTree


//Going down left/right subtree
goL :: (Tree a) -> (Tree a)
goL (Node x l r) = l
goR :: (Tree a) -> (Tree a)
goR (Node x l r) = r
//Start = goL ourTree

//Checking if we're at a leaf
isLeaf :: (Tree a) -> Bool
isLeaf Leaf = True
isLeaf _ = False
//Start = isLeaf emptyTree

/*
isLeaf x
| x == Leaf = True
| x <> Leaf = False
*/


//Get a list of subtrees from a node.
getSubTrees :: (Tree a) -> [(Tree a)]
getSubTrees Leaf = []
getSubTrees (Node x l r) = (getSubTrees l) ++ [l] ++ [r] ++ (getSubTrees r)
//Start = getSubTrees ourTree

//Get the min value of a BST
minTree :: (Tree a) -> a
// minTree (Node x l r) 
// | isLeaf l = x
// = minTree l

minTree (Node x Leaf _) = x
minTree (Node _ l _) = minTree l
//Start = minTree messyTree
// minTree t
// | isLeaf (goL t) = getNode t 
// = minTree (goL t)
//Start = minTree ourTree

//Reverse a tree
reverseTree :: (Tree a) -> (Tree a)
reverseTree Leaf = Leaf
reverseTree (Node x l r) = (Node x (reverseTree r) (reverseTree l))
// reverseTree t 
// | isLeaf t = Leaf
// = (Node (getNode t) (reverseTree(goR t)) (reverseTree(goL t)))
//Start = reverseTree ourTree

//Get the max value of a BST
maxTree :: (Tree a) -> a
maxTree t = minTree(reverseTree t)
//Start = maxTree ourTree


//Extract sublists countaining a specific element

//Start = extractSubLists 3 ourTree

//Get a list of children of a node
getChildren n Leaf = []
getChildren n (Node x Leaf Leaf) = []
getChildren n (Node x l r)
| x == n = [getNode l, getNode r]
| x < n = getChildren n l 
| x > n = getChildren n r
//Start = getChildren 15 specialTree 

//Get the parent of a node

//Start = findParent 13 ourTree
//Start = findParent 15 ourTree
//Start = findParent 19 ourTree

//Check if a Binary Tree is actually a BST

//Start = checkBST ourTree
//Start = checkBST messyTree

//Add a new node to a BST
//addNode :: (Tree a) a -> (Tree a) | < a
//addNode Leaf x = (Node x Leaf Leaf)
//addNode t x
//| x < (getNode t) = (Node (getNode t) (addNode (goL t) x) (goR t))
//| x > (getNode t) = (Node (getNode t) (goL t) (addNode (goR t) x))
//= t

//Start = addNode  singleTree 17
getchild a Leaf = Leaf
getchild a (Node x l r)
|a == x = (Node x l r)
|a < x = getchild a l
=getchild a r

//Start = getchild 1 ourTree
//This fun will add New node in a tree
addNode n Leaf = Leaf
addNode n (Node x l r)
|n==x = (Node n l r)
|n<x = addNode n l
= addNode n r
Start = addNode 16 ourTree


