module third
import StdEnv
/*
3.Write a function which gets a number K and list of numbers and divides it into sublists
of size K. Last partition may have smaller size than K. After that sort each sublist and
return list of lists.
Ex.: K = 3, List = [1,7,3,5,2,3,11,8] -> [[1,7,3], [5,2,3], [11,8]]
-> [[1,3,7],[2,3,5],[8,11]]
*/

listDivider :: Int [Int] -> [[Int]]
listDivider a [] =[]
listDivider a listi = [sort (take a listi)] ++ listDivider a (drop a listi)
//Start = listDivider 3 [1,7,3,5,2,3,11,8] // [[1,3,7],[2,3,5],[8,11]]
// Start = listDivider 1 [5,4..2] // [[5],[4],[3],[2]]
// Start = listDivider 4 [5,4..2] // [[2,3,4,5]]
// Start = listDivider 5 ([7,6..2] ++ [1..5] ++ [5,7..11]) // [[3,4,5,6,7],[1,2,2,3,4],[5,5,7,9,11]]