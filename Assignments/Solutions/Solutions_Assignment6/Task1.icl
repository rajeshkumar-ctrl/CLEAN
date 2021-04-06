
/*1. Given two sorted lists of unique integers, return lists which contains their union.
Union of the two lists should contain all integers which appear in at least one of them.
Each element should be added only once. Elements in union should be sorted in ascending order.
Ex.: [1,2,3,4,5] [4,5,6,7] -> [1,2,3,4,5,6,7]
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
