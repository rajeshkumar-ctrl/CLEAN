module Task7
import StdEnv




/*
Given a list of integers, for each element do the following:
if the element is even, check if this integer appears even amount of times in the list
if the element is odd, check if this integer appears odd amount of times in the list
*/

checkEven :: Int [Int] -> Bool
checkEven x [] = False
checkEven x list = isEven (length (filter ((==) x) list))

check :: [Int]-> [Bool]
check [] = [False]
check list = [checkEven (x) list \\ x<-list ]

//Start = check [] // []
//Start = check [1,2,2,1] // [False,True,True,False]
//Start = check [1,1,1,2,2,2,3,5,3,3,5] // [True,True,True,False,False,False,True,False,True,True,False]
