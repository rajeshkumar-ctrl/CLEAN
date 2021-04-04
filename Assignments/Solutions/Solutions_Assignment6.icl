module asix
import StdEnv



/*1. Given two sorted lists of unique integers, return lists which contains their union.
Union of the two lists should contain all integers which appear in at least one of them.
Each element should be added only once. Elements in union should be sorted in ascending order.
Ex.: [1,2,3,4,5] [4,5,6,7] -> [1,2,3,4,5,6,7]
*/
/*
listUnion :: [Int] [Int]-> [Int]
listUnion [][]=[]
listUnion [x:xs] [y:ys] = [x: listUnion  ]

*/


listUnion :: [Int] [Int] -> [Int]
listUnion listone listtwo =  removeDuplicates (sort([x \\x<-listone ] ++ [y \\ y<-listtwo]))

toRemove :: Int [Int] -> [Int]
toRemove x [] = []
toRemove x [y:ys]
| x == y = toRemove x ys
| otherwise = [ y : toRemove x ys ]

removeDuplicates :: [Int] -> [Int]
removeDuplicates [ ] = [ ]
removeDuplicates [x:xs] = [ x : removeDuplicates (toRemove x xs ) ]


//Start = listUnion [1,3,4,7,8,12,13] [1,4,6,10,11,12,15] // [1,3,4,6,7,8,10,11,12,13,15]
// Start = listUnion [1..5] [1,3..10] // [1,2,3,4,5,7,9]
// Start = listUnion [1,2] [] // [1,2]
// Start = listUnion [] [] // []

/*2. Given the list of points and a distance. Each point is represented with tuple, containing X and Y coordinates
in 2D plane. Return how many pair of pointrs are there so that, distance between them is equal to the given value.
*/


pointDistance :: [(Int,Int)] -> Int
pointDistance [(_,_)] = 0
pointDistance list = countero ([point ((hd list),tuple) \\tuple<-list]++(pointDistance (drop 1 list)))


countero :: [Int] -> Int
countero [] = 0
countero [x:xs] = 1 +countero xs

Start = pointDistance [(1,1), (4,5), (8,8), (10,3)] 
//new :: [(Int,Int)] -> [Int]
//new [(_,_)]=[]
//new list = [  point (tuple,tuple) \\ tuple<-list]


//Start = new [(1,1), (4,5), (8,8), (10,3)]

point :: ((Int,Int),(Int,Int)) -> Int
point ((a,b),(c,d)) = ((c-b)*(c-b))+((d-a)*(d-a)) 

//Start = point ((1,1),(4,5))

//Start = fst(hd ([(1,1), (4,5), (8,8), (10,3)] )) *  fst(hd (drop 1 ([(1,1), (4,5), (8,8), (10,3)] )) )// 2

/*


Start = counter [1,2,3,4,5]

*/



// Start = pointDistance [(1,1), (4,5), (8,8), (10,3)] 5 // 2
// Start = pointDistance [(1,1), (4,5), (8,8), (10,3)] 2 // 0
// Start = pointDistance [(3,4), (3,8), (7,8)] 4 // 2
// Start = pointDistance [] 3 // 0
// Start = pointDistance [(1,1)] 2 //


/*3. Given the list of integers, modify it in a following way:
I. Remove all numbers which are multiple of 3
II. Sort remaining list in descending order
III. Swap 1st and 2nd elements, 3rd and 4th, 5th and 6th and so on.
*/


shuffleSort :: [Int] -> [Int]
shuffleSort [] = []
shuffleSort list = reverse(take 2 (removethree (list))) ++ shuffleSort (drop 2 (removethree (list)))


removethree :: [Int]-> [Int]
removethree list = reverse (sort([x \\ x <- list | x rem 3 <> 0]))





//Start = removethree [4,1,3,2,5,6,7] // [2,4]

// Start = shuffleSort [4,3,2] // [2,4]
//Start = shuffleSort [7,5,4,2,1] // [5,7,2,4,1]
// Start = shuffleSort [3,6,3,9,12] // []
//Start = shuffleSort [2,4,5,7,14,17] // [14,17,5,7,2,4]
// Start = shuffleSort [] // []
