module midone
import StdEnv 

//Task 01
//Define a function which finds the maximum of two numbers
maximum :: Int Int -> Int
maximum a b
| a > b = a
= b

//Start = maximum 3 4 // 4


///Task 02
//Given a list of Reals.
//Write a code which will add 1 to every real number from the list which is less than 10

//Recursive Function 
subst2 :: [Real] -> [Real]
subst2 [] = []
subst2 [a:b]
| a < 10.0 = [a+1.0 : subst2 b]
= [a: subst2 b]
//Start=subst2 [1.6,12.4,5.4,12.4] //[2.6,12.4,6.4,12.4]


//Task03
//Write a function which takes a [Int] and returns a
//[Int] containing the middle element of that list.
//Note: lists with odd number of elements will only
//return a list with one middle element, lists with
//even number of elements should return a list with
//two elements.


middle::[Int]-> [Int]
middle [] = [-1]
middle list 
| (length list) rem 2 == 0 = [list!!(halfLen-1), list!!(halfLen)]
= [list!!halfLen]
where 
	halfLen = ((length list)/2)
//Start=middle [1..10]//[5,6]
//Start=middle [1,2,3,4,5]//[3]
//Start=middle []//[-1]



//Task04
/*
Given a list of numbers increase all the elemnts by one using higher order function
*/
increment:: [Int] -> [Int]
increment list = map (inc) list //Note inc is not a higher order function, map is

inc2 :: [Int] -> [Int]
inc2 list = map (\x -> x+1) list // This is Perfectly fine as well

//Start = inc2 [1..5] //[2,3,4,5,6]
//Start = increment [1..5] //[2,3,4,5,6]
//Start = increment [-5..5] //[-4,-3,-2,-1,0,1,2,3,4,5,6]
//Start = increment []//[]


//Task05
//Given a list of Int.
//Write a function which will calculate the sum of the numbers up to the first number greater than 10*/
sumTillGreater::[Int]->Int
sumTillGreater list = sum(takeWhile(\x -> x <=10) list)
//Start=sumTillGreater [1,4,10,12]//15
//Start=sumTillGreater [1,2,3,4,5,6,7]//28
//Start=sumTillGreater []//0
//Start=sumTillGreater [10,11]//10





///Task06
/*
Given a list of Tuples of Integer, give a list of integers produced if we raise
the first integer to the power of the second integer and keep only the even numbers.
Example :
[(2,4),(3,2)] --->[16]
because (2,4) ->2^4 = 16 and is even
(3,2) -> 3^2=9 is odd
*/
power :: [(Int,Int)] ->[Int]
power listOfTuples = [((fst tuple)^(snd tuple))\\tuple<-listOfTuples | isEven ((fst tuple)^(snd tuple)) ]

//Start = power [(2,4),(3,2)]//[16]
//Start=power [(1,100),(2,3),(4,5)]//[8,1024]
//Start = power [(3,5),(7,9)]//[]


/*
1. Please write a function, where given an Int 'n', you return the nth Fibonacci number.
2: Make a list of infinite Fibonacci numbers.
3: Make a list only containing only Fibonacci numbers generated from prime integers. 
4: Take from the list of primes indexed Fibonacci list
the Fibonacci numbers that are less than an Int x.
INFO: A Fibonacci number is calculated from a sequence start with 1 and 1, and each 
successive number is the sum of the previous two numbers.
A prime number is a number that has no divisors except 1 and itself.
*/

Fib :: Int -> Int
Fib 0=0
Fib 1=1
Fib a = (Fib(a-2)+Fib(a-1))

fibList :: [Int]
fibList = [Fib a\\a<-[1..20]| prime a]

prime :: Int -> Bool
prime x = and[x rem element <> 0 \\ element<-[2..x-1]]

fibo :: Int -> [Int]
fibo secret= takeWhile (\x -> x<=secret) fibList
//Start =fibo 10
