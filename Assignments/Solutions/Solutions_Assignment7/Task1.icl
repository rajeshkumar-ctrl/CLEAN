module Task1
import StdEnv
/* 1. Write a function that takes an array of integers and gives back a tuple that contains:
(the integer in the array, a boolean value)
the boolean value tells if when cutting the integer in half it consists of
the same number, e,g, 2020 -> 20 20 so it keeps it but 2008 -> 20 08 it doesn't.
*/

toTuple :: {Int} -> {(Int, Bool)}
toTuple list = {(x, CheckFunction x) \\ x <-:list} 
	
CheckFunction :: Int -> Bool
CheckFunction x = take ((length (NumIntoList x)) / 2) (NumIntoList x) == drop ((length (NumIntoList x)) / 2) (NumIntoList x)

NumIntoList :: Int -> [Int]
NumIntoList x
| x < 10 = [x]
= [x rem 10] ++ NumIntoList (x / 10)


// Start = toTuple {} // {}
//Start = toTuple {100, 2020, 1919} // {(100,False),(2020,True),(1919,True)}
// Start = toTuple {312, 1001, 1010} // {(312,False),(1001,False),(1010,True)}
