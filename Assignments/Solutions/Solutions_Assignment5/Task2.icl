/*
2. Given lists of lists, process each list and for each number in the list
change their value with the number of times it appears in this list.
Ex.: [[1,1,2,1,2,3], [2]] -> [[3,3,2,3,2,1], [1]]
1 appears 3 times in the first list, so each 1 is changed with 3.
2 appears 2 times in the first list, so each 2 is changed with 2.
3 appears 1 times in the first list, so each 3 is changed with 1.
2 appears 1 times in the second list, so each 2 is changed with 1.
*/


count :: [Int] Int -> (Int,Int)
count list x =  (x,length[element\\element<-list | element==x])

secondcount::[Int] -> [Int]
secondcount list = [snd(count list e)\\e <- list]

counter :: [[Int]] -> [[Int]]
counter listoflists = [secondcount list \\ list <-listoflists]



//Start = counter [[1,1,2,1,2,3], [2]] // [[3,3,2,3,2,1],[1]]
// Start = counter [[1,2,1,1,3,4,3],[2,2,4,3,2,2,1]] // [[3,1,3,3,2,1,2],[4,4,1,1,4,4,1]]
// Start = counter [[1..10]] // [[1,1,1,1,1,1,1,1,1,1]]
// Start = counter [] // []
// Start = counter [[1,2,2,3], [], [1,3,4,3,3,1,4]] // [[1,2,2,1],[],[2,3,2,3,3,2,2]]
