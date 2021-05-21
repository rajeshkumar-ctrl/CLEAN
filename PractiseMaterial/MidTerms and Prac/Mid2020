module Mid2020
import StdEnv

/*________________________[Clean Mid Term Exam 2020, ELTE]___________________________*/



//Wave 01 
/*
Grade 2: Please write a function, where given an Int 'n', you return the nth Fibonacci number.
Grade 3: Make a list of infinite Fibonacci numbers.
Grade 4: Make a list only containing only Fibonacci numbers generated from prime integers. 
Grade 5: Take from the list of primes indexed Fibonacci list
the Fibonacci numbers that are less than an Int x.
INFO: A Fibonacci number is calculated from a sequence start with 1 and 1, and each 
successive number is the sum of the previous two numbers.
A prime number is a number that has no divisors except 1 and itself.
*/

Fib :: Int -> Int
Fib 0 = 0
Fib 1 = 1
Fib n = Fib (n-1) + Fib (n-2)


//Start = Fib 5 //5
//Start = Fib 8 //21
//Start = Fib 20 //6765

FibList :: [Int]
FibList = [Fib x\\x<-[1..]]

//Start = take 10 FibList //[1,1,2,3,5,8,13,21,34,55]
//Start = drop 10 (take 20 FibList) //[89,144,233,377,610,987,1597,2584,4181,6765]

isPrime :: Int -> Bool
isPrime x = and[x rem elem <> 0\\elem<-[2..x-1]]


PrimeFibList :: [Int]
PrimeFibList = [Fib x\\x<-[1..] | isPrime x]
//Start = take 10 PrimeFibList //[1,1,2,5,13,89,233,1597,4181,28657]

crazyFibList :: Int -> [Int]
crazyFibList num = takeWhile (\x -> x <= num) PrimeFibList



//Start = crazyFibList 100 //[1,1,2,5,13,89]
//Start = crazyFibList 9001 //[1,1,2,5,13,89,233,1597,4181]






/*________________________________________________________________________________________________________________________________________________________________________*/


///Wave 02 

/*
Grade 2 : Given a real number, find if it is negative
Grade 3 : Given a list of real numbers, remove all negative numbers    
Grade 4 : Given a list of real numbers, return list which contains square root of all non-negative numbers
Grade 5 : Given a number, find roots from all positive numbers which are multiple of 3 and less or equal to the given number.
*/



isNegative:: Real -> Bool
isNegative num = num < 0.0

//Start = isNegative 1.2 // False
//Start = isNegative -0.3 // True 
//Start = isNegative 0.0 // False



remNegatives:: [Real] -> [Real]
remNegatives [] = []
remNegatives [x:y]
| isNegative x = remNegatives y
= [x: remNegatives y]


//Start = remNegatives [1.0,2.5,-3.2,-1.5,3.0] // [1.0,2.5,3.0]
//Start = remNegatives [-1.0,-0.5,-3.4] // []
//Start = remNegatives [0.0,2.5,-1.3] // [0.0,2.5]

getRoots:: [Real] -> [Real]
getRoots list = map (\x -> sqrt x) (filter (\x -> not(isNegative x)) list) 


//Start = getRoots [25.0,-3.4,1.0] // [5.0,1.0]
//Start = getRoots [-1.0,-0.5,-3.4] // []
//Start = getRoots [25.0, 16.0, 4.0, 100.0] // [5.0, 4.0, 2.0, 10.0]




getRootsWhich:: Int -> [Real]
getRootsWhich n = [sqrt(toReal elem)\\elem <- takeWhile (\x -> x <= n) [1..] | elem rem 3 == 0]

//Start = getRootsWhich 0 // []
//Start = getRootsWhich 10 // [1.73205080757,2.44948974278,3]
//Start = getRootsWhich 2 // []
//Start = getRootsWhich 21 // [1.73205080757,2.44948974278,3,3.46410161514,3.87298334621,4.24264068712,4.58257569496]




/*_____________________________________________________________________________________________________________________________________________________________________*/





/*
Grade 2 : Please write a function to decide if its input is Integer or not. (No type signature needed)
Grade 3 : Modify your function to decide if it its input is natural number.
Grade 4 : Please write a function to decide if a number is a perfect square.
Grade 5 : Please write a function to decide if a all the numbers in a list are perfect square.
INFO : Perfect square number is an integer equals to a square of another integer.
i.e : 9 is a perfect square since 3^2 = 9
*/


///Well, in mathematics the number zero is not an natural number 

isInt num 
| toReal num < 0.0 = False 
= toReal num == toReal (toInt (toReal num))
//Start = toReal 2.3 == toReal(toInt(toReal 2.3))
//Start = isInt 2 //True 
//Start = isInt 2.6

//Start = isInt 0
isNat num 
| toReal num <= 0.0 = False 
= toReal num == toReal (toInt (toReal num))


//Start = isNat 4 // True
//Start = isNat 4.1 // True  ///this is the examinar's fault: 4.1 is not a natural number. 4.1 is a Real 
//Start = isNat -1 // False

isPerfectSquare :: Int -> Bool
isPerfectSquare num = isNat(sqrt (toReal num)) 

//Start = isPerfectSquare 9 // True
//Start = isPerfectSquare 1 // True
//Start = isPerfectSquare 0 // True
//Start = isPerfectSquare 17 // False
//Start = isPerfectSquare -1 // False

areAllPerfectSquares :: [Int] -> Bool
areAllPerfectSquares list = and[isPerfectSquare x\\x<-list]
//Start = areAllPerfectSquares [1, 9, 36, 16] // True
//Start = areAllPerfectSquares [2, 4, 34] // False



/*__________________________________________________________________________________________________________________________________________________________________________*/





//This was my wave and I got ZERO!!!! :(

/*
Grade 2 : Write a function to count how many digits in a number
Grade 3 : Modify the previous function to only count the digits of the Positive number (gives 0 otherwise)
Grade 4 : Write a function to decide if a number is a Magic number.
Grade 5 : Write a functio to determine if all the Integers in the list are magic numbers.
INFO : Magic number has a special properity the first n of its digits, 
should be multiple of n.
Note : 0 is not a magic number
i.e : 120 is Magic number since 12 is multiple of 2 and 120 is multiple of 3. 
On the other hand 118 is not since 11 is not multiple of 2.
*/ 

///// Recursion /////
countNum :: Int -> Int
countNum num 
| num < 10 = 1
= countNum(num/10) + 1

//// Lists ////

countNum01 :: Int -> Int
countNum01 num = length(countNumAux num)

countNumAux :: Int -> [Int]
countNumAux 0 = []
countNumAux num 
= [num rem 10] ++ countNumAux(num/10)

//Start = countNum01 1234
//Start = countNum 1234567710
//Start = countNum 1234 // 4
//Start = countNum 0 // 1
//Start = countNum -1 // 1


cntPos :: Int -> Int
cntPos num
| num < 0 = 0 
| num < 10 = 1
= cntPos(num/10) + 1

//Start = cntPos -1234 //0


///It's all about recursion ////
isMagicNum :: Int -> Bool
isMagicNum 0 = False
isMagicNum num 
| num < 10 = True
= (num rem (countNum num) == 0) && isMagicNum (num/10)
//Start = isMagicNum 0 // False
//Start = isMagicNum 4 // True
//Start = isMagicNum 14 // True
//Start = isMagicNum 120 // True
//Start = isMagicNum 118 // False

areAllMagicNums :: [Int] -> Bool
areAllMagicNums list = and[isMagicNum x\\x<-list]

//Start = areAllMagicNums [4, 14, 120] // True
//Start = areAllMagicNums [4, 51, 6] // False



/*_________________________________________________________________________________________________________________________________________________________________________*/


///Da yo question warky extra ya kha v, mara 
//This question was not inculded in the exam
/*
Insert x as second element in every sublist of a list.
If the sublist was empty then x will be the only element in the new sublist.
[[1,2], [3,4,5], [6,5,9,7], [], [8]] 10 -> [[1,10,2], [3,10,4,5], [6,10,5,9,7], [10], [8,10]]
*/

secondPlaceIns:: [Int] Int-> [Int]
secondPlaceIns [] x = [x]
secondPlaceIns [a:b] x = [a,x:b]

Ins :: [[Int]] Int-> [[Int]]
Ins listOflists num = [secondPlaceIns x num\\x<-listOflists] 
//Start = Ins [[1,2], [3,4,5], [6,5,9,7], [], [8]] 10
