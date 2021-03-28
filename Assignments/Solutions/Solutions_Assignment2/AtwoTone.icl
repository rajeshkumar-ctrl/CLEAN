module AtwoTone
import StdEnv

//1. Give a list of numbers, multiplying all even numbers by 2 and all odd numbers by 3




multiply :: [Int] -> [Int]

multiply [] = []
multiply [x : xs]

|x rem 2 == 0 = [2 * x : multiply xs]
=[3 * x : multiply xs]

Start = multiply [1,2,3,4,5]
//Start = multiply [14, 22, 45, 56] // [28, 44, 135, 112]
//Start = multiply [13, 27, 44] // [39, 81, 88]
//Start = multiply [] // []
