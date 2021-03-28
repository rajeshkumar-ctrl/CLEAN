
module task
import StdEnv



/*
2.Write a function which takes 2 list of integers and 1 list of booleans and returns
a single list of tuples, where i-th tuple contains i-th elements from input lists.
Returned list's size should be equal to the shortest input list's size.
*/

tripleZip :: [Int] [Int] [Bool] -> [(Int,Int,Bool)]
tripleZip [][][] = []
tripleZip listt listz listy = [(x,y,z)\\x<-listt&y<-listz&z<-listy]

//Start = tripleZip [1,5,7] [4,10,2,3] [False, True, True] // [(1,4,False),(5,10,True),(7,2,True)]
// Start = tripleZip [1..10] [11..20] [False \\ x <- [1..50]] // [(1,11,False),(2,12,False),(3,13,False),(4,14,False),(5,15,False),(6,16,False),(7,17,False),(8,18,False),(9,19,False),(10,20,False)]
// Start = tripleZip [1,3..20] [4,10,2,3] [False, True, True] // [(1,4,False),(3,10,True),(5,2,True)]
// Start = tripleZip [] [11..20] [True \\ x <- [1..15]] // []