module AtwoTthree
import StdEnv


/*
same :: [Int] [Int] -> Bool 
same [ ] [ ] = True
same [x:xs] [y: ys]
| length [x:xs] <> length [y:ys] = False
| (x rem 2 == 0 && y rem 2 == 0) || (x rem 2 <> 0 && y rem 2 <> 0) = True && same xs ys
= False 

*/

/*
same :: [Int] [Int] -> Bool 
same [] [] = True 
same l1 l2 
| length l1 <> length l2 = False
| (hd l1 rem 2 == 0 && hd l1 rem 2 == 0) || (hd l1 rem 2 <> 0 && hd l2 rem 2 <> 0 ) = True && same (drop 1 l1) (drop 1 l2)
= False

*/

//Start = same [1,2,3] [2,4,6] // False
//Start = same [1,2,3,4] [3,8,5,12] // True
//Start = same [] [] // True