/*
1. Write a function which takes a list of numbers and returns tuple
of two lists: I. Even numbers from the list sorted in ascending order.
II. Odd numbers from the list sorted in descending order.
Ex. [1,3,2,7,12,4,1,4,5,12,9,4] -> [2,4,4,4,12,12] [9,7,5,3,1,1]
*/


splitSort :: [Int] -> ([Int],[Int])
splitSort [] = ([],[])
splitSort list = (sort [ a \\ a<-list | (isEven a)], reverse ( sort [b \\ b<-list |(isOdd b)] ))




//Start = splitSort [1,3,2,7,12,4,1,4,5,12,9,4] // ([2,4,4,4,12,12],[9,7,5,3,1,1])
// Start = splitSort [1,2..13] // ([2,4,6,8,10,12],[13,11,9,7,5,3,1])
// Start = splitSort [2,4..14] // ([2,4,6,8,10,12,14],[])
// Start = splitSort [] // ([],[])
