//ITS INCOMPLETE

module lasthomework
import StdEnv


:: University = ELTE | BME | Corvinus
:: Student = {name::String, uni :: University, grades:: [Int]}
 

Rose::Student
Rose = {name="Rose",uni=ELTE, grades =[5,5,3,4,2,4,5,5]}
Peter::Student
Peter = {name="Peter",uni=BME, grades =[3,2,3,4,2,4,2,1,4,3,2,4]}
Noah::Student
Noah = {name="Noah",uni=Corvinus,grades=[1,2,2,3,1,3,4,2,3,4,2,4,2,1]}
James::Student
James = {name="James",uni=ELTE,grades=[5,5,5,5,3,4,5,4,5]}
Lily::Student
Lily = {name="Lily",uni=BME,grades=[1,2,1,3,1,5,3,3,4,1,3,1,5,1,1]}
Harry::Student
Harry = {name="Harry",uni=Corvinus,grades=[3,4,1,3,4,2,3,5,5]}
Eros::Student
Eros = {name="Eros",uni=Corvinus,grades=[4,2,4,4,4,4,4,5,2]}
Isabella::Student
Isabella = {name="Isabella",uni=BME,grades=[5,5,5,4,5,5,4,5,4,5]}
Oliver::Student
Oliver = {name="Oliver",uni=ELTE,grades=[2,3,3,4,3,2,1,3,2,3]}
 

/* 1.
Given array of students, find the University which has highest
average of student average GPA.
 

Example:
{Peter, Eros, Harry}
Peter's average GPA - 2.83
Eros's average GPA - 3.67
Harry's average GPA - 3.33
 

Hence:
ELTE's average grades - []
BME's average grades - [2.83]
Corvinus's average grades - [3.67, 3.33]
 

Corvinus has highes averafe - 3.5
*/
 

uniWithHighestAverage :: {Student} -> University

Gpa :: Student -> Real
Gpa {grades=g} = Average

where
    Average = (toReal(sum g)) /(toReal (length g)) 


UniGpa :: University -> [Real]
UniGpa {Student=a} = map Gpa a

where
    a = [x\\x<-:a]
    Average = (toReal(sum a)) /(toReal (length a)) 

//Start = UniGpa ELTE //[4.25,3.6,4.66666666666667,3.5]



/*
uniWithHighestAverage :: {University} -> String
uniWithHighestAverage array
| len == 0 = "No universities given"
| name == "Empty" = "Empty"
| maxGpa == Gpauni = x.uni
= uniWithHighestAverage arrminus1
where
    list = [e\\e<-:array]
    maxGpa = maxList (flatten (map UniGpa list))
    Gpauni = (maxList (UniGpa x))
    x = (hd list) 
    xs = (tl list)
    arrminus1 = {e\\e<-xs}
    name = x.uni
    len = (length list)

*/

// THis is limit of my brain i cannot think anymore!




// Start = uniWithHighestAverage {Rose,Harry,Isabella,Oliver,James,Noah,Lily,Peter,Eros} // ELTE
// Start = uniWithHighestAverage {Rose,Harry,Isabella} // BME
// Start = uniWithHighestAverage {Oliver, Noah,James,Lily} // ELTE
// Start = uniWithHighestAverage {Peter, Eros, Harry} // Corvinus

/* 2.
Define an instance of the built-in class ==
for Student. Students are equal if they have same
name and are from the same university.
*/
 
instance == Student
where
    == a b = a.name == b.name && a.uni == b.uni 

instance == University 
where
    == ELTE ELTE = True
    == BME BME = True
    == Corvinus Corvinus = True
    == _ _= False


//Start = Rose == Harry // False
//Start = Harry == Harry // True
//Start = {name="John", uni=ELTE, grades=[]} == {name="John", uni=ELTE, grades=[1,2,3]} // True
 

/* 3.
You are given a binary tree.
Check if it is a binary search tree (BST).
In BST values in left subtree should be
less then the current node's value and
values in right subtree should be greater.
*/
 

/*
           8
         /   \
        4     9
       / \   / \
      L   5  L  L
*/     


:: BST a = BSTNode a (BST a) (BST a) | BSTLeaf

BSTgetNode :: (BST Int) -> Int
BSTgetNode (BSTNode x l r)  = x

BSTLeftIter :: (BST Int) -> (BST Int)
BSTLeftIter (BSTNode x l r) = l

BSTRightIter :: (BST Int) -> (BST Int)
BSTRightIter (BSTNode x l r) = r  //strictly greater

//in a sorted manner => ascending order.
BSTToList :: (BST Int) -> [Int]
BSTToList BSTLeaf = []
BSTToList tree = BSTToList(BSTLeftIter tree) ++ [BSTgetNode tree] ++ BSTToList(BSTRightIter tree)

isBST :: (BST Int) -> Bool
isBST BSTLeaf = True
isBST tree = BSTToList tree == sort (BSTToList tree)
 


// bst1 = (BSTNode 1 BSTLeaf (BSTNode 20 (BSTNode 3 (BSTNode 3 BSTLeaf BSTLeaf) (BSTNode 4 BSTLeaf (BSTNode 12 (BSTNode 5 BSTLeaf BSTLeaf) BSTLeaf))) (BSTNode 45 (BSTNode 34 (BSTNode 22 BSTLeaf BSTLeaf) BSTLeaf) (BSTNode 112 (BSTNode 53 BSTLeaf BSTLeaf) BSTLeaf))))
// bst2 = (BSTNode 1 BSTLeaf (BSTNode 20 (BSTNode 7 BSTLeaf (BSTNode 12 (BSTNode 12 (BSTNode 9 BSTLeaf BSTLeaf) BSTLeaf) BSTLeaf)) BSTLeaf))
// bst3 = (BSTNode 1 BSTLeaf (BSTNode 20 (BSTNode 3 (BSTNode 9 BSTLeaf BSTLeaf) (BSTNode 4 BSTLeaf (BSTNode 1 (BSTNode 8 BSTLeaf BSTLeaf) BSTLeaf))) (BSTNode 45 (BSTNode 34 (BSTNode 22 BSTLeaf BSTLeaf) BSTLeaf) (BSTNode 112 (BSTNode 53 BSTLeaf BSTLeaf) BSTLeaf))))
// bst4 = (BSTNode 1 BSTLeaf (BSTNode 2 (BSTNode 7 BSTLeaf (BSTNode 12 (BSTNode 12 (BSTNode 8 BSTLeaf BSTLeaf) BSTLeaf) BSTLeaf)) BSTLeaf))
 

// Start = map isBST [bst1,bst2,bst3,bst4,BSTLeaf] // [True,True,False,False,True]
