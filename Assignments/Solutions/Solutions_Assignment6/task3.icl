
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



// Start = shuffleSort [4,3,2] // [2,4]
//Start = shuffleSort [4,1,3,2,5,6,7] // [5,7,2,4,1]
// Start = shuffleSort [3,6,3,9,12] // []
// Start = shuffleSort [2,4,5,7,14,17] // [14,17,5,7,2,4]
// Start = shuffleSort [] // []
