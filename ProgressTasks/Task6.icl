module Task6
import StdEnv


intersection :: [Int] [Int] -> [Int]
intersection [] _ = []
intersection _ [] = []
intersection listone listtwo 
| (hd listone == hd listtwo) = [hd listone] ++ intersection (drop 1 listone) (drop 1 listtwo)
= intersection (drop 1 listone) (drop 1 listtwo)

//Start = intersection [1,2,3,4] [1,4,3,5] // [1,3]
// Start = intersection [1..5] [1..5] // [1,2,3,4,5]
// Start = intersection [1,3,2,3,4,6] [1,2,2,2,3,6] // [1,2,6]
// Start = intersection [] [1..4] // []
// Start = intersection [1..10] [2..7] // []
