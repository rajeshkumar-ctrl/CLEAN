module second
import StdEnv




/* 2.
Write a function that takes two integers a and b.
Use iterate to take first a number of elements from the list
which contains the result of b^n (n goes from 0 to a).
Return an empty list for illegal integers a or b.
*/
power :: Int Int -> [Int]
power a 0 = [ ]
power a b = [ pow b a \\ a <-[0..a-1]]


pow :: Int Int -> Int
pow a b
| b == 0 = 1
= a * pow a (b-1)

//Start = power 100 0 // []
//Start = power 10 2 // [1,2,4,8,16,32,64,128,256,512]
//Start = power 15 3 // [1, 3, 9, 27, 81, 243, 729, 2187, 6561, 19683, 59049, 177147, 531441, 1594323, 4782969]