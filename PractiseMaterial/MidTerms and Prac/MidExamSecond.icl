module MidTerm02
import StdEnv 



//1. Write a function that will return the second to last digit in a number. Return 0 if there is no second digit.
f1 :: Int -> Int
f1 n = (n/10) rem 10

//Start = f1 1234 //3
//Start = f1 5 //0
//Start = f1 ~(5564) //6


/*________________________________________________________________________________________________________________________________________*/




// 2. Write a function that will subtract numbers in a list from the first one. Your solution must use 'foldr' or 'foldl'.
// Return 0 for an empty list.


///// Using foldl is easy because it starts from left. That way we can start from the first elements easily/////
f2 :: [Int] -> Int
f2 [] = 0
f2 [first:rest] = foldl (-) first rest


//Start = f2 [10,1,2,3] //4
//Start = f2 [1,2,3,4] //-8
//Start = f2 [1000,500,250,125] //125
//Start = f2 [] //0





/*__________________________________________________________________________________________________________________________________________*/





// 3. Write a function that returns all prime divisors of a number. e.g. f3 36 = [1,2,3]

//////*   Most beautiful isPrime function     *//////////
isPrime :: Int -> Bool
isPrime n 
| n >= 1  = and[n rem x <> 0\\x<-[2..toInt(sqrt(toReal n))]] 
= False

///// Prime Factorization algorithm from my shell script class ///////
anyfunc :: Int Int -> [Int]
anyfunc 1 _ = [1]
anyfunc num x
| isPrime num = [1,num] 
| num rem x == 0 = removeDup(sort([x] ++ anyfunc(num/x) x))
= removeDup(sort(anyfunc (num) (x+1))) 

//This function right here is the redundancy of my naive approach/a very sophisticated approach :) 
callanyFunc :: Int -> [Int]
callanyFunc x = anyfunc x 2 

//we could've solved this with list comp or filter, but that is not an effective solution for big numbers coz 
//you have to search each and every single number between 1 and the number whether it divides the number or not, 
//and if it divides then you have to check if it is a prime or not and for that itself you are using list comp, so it will
//take a lot of time to compute
f3 :: Int -> [Int]
f3 num 
| num > 0 = callanyFunc num
= []


//Start = f3 36 //[1,2,3]
//Start = f3 524287  //[1,524287]
//Start = f3 0 //[]





/*__________________________________________________________________________________________________________________________________________*/





//4. Write a function that reverses tuples from a list if the tuple members sum up to an even number.

f4 :: [(Int, Int)] -> [(Int, Int)]
f4 [] = []
f4 [(a,b):rest]
| isEven(a+b) = [(b,a):f4 rest]
= [(a,b):f4 rest]


//Start = f4 [(1,2),(3,4),(5,7)] //[(1,2),(3,4),(7,5)]
//Start = f4 [(-1,3),(12,1),(0,0),(-4,-2)] //[(3,-1),(12,1),(0,0),(-2,-4)]
//Start = f4 [] //[]






/*____________________________________________________________________________________________________________________________________________*/




//5. Write a function that takes every number in a list and generates a sublist of its first 5 multiples. Your solution must use 'map'.

f5 :: [Int] -> [[Int]]
f5 list = [take 5 (map (\e -> e*x) [1..])\\x<- list ]
//Start = f5 [1..3] //[[1,2,3,4,5],[2,4,6,8,10],[3,6,9,12,15]]
//Start = f5 [4,~3,5,~6] //[[4,8,12,16,20],[-3,-6,-9,-12,-15],[5,10,15,20,25],[-6,-12,-18,-24,-30]]
//Start = f5 [] //[]






/*_____________________________________________________________________________________________________________________________________________*/






// 6. Given an integer n, find the minimal k such that
// k = m! (where m! = 1 * 2 * ... * m) for some integer m; k >= n.
// In other words, find the smallest factorial which is not less than n.
// example: leastfactorial 17 = 24.  because 17 < 24 = 4! = 1 * 2 * 3 * 4, while 3! = 1 * 2 * 3 = 6 < 17

////// Generate a list of all the factorials from one to that number [1,2,6,24,120,720..] then use dropWhile utility /////
leastfactorial :: Int -> Int
leastfactorial n = hd(dropWhile (\x -> x < n)[prod[1..x]\\x<-take n [1..]])
//Start = leastfactorial 17 // 24
//Start = leastfactorial 1 // 1
//Start = leastfactorial 5 // 6
//Start = leastfactorial 25 // 120




/*_______________________________________________________________________________________________________________________________________________*/




// 7. Write a function that checks if a list of numbers is odd,even,odd,even...
// e.g. for [1,2,3,4,6] it is false because 4 is even, but 6 is not odd.

f7 :: [Int] -> Bool
f7 [] = False
f7 list = and[isEven(x+y)\\x <- list & y<- [1..]]

//Start = f7 [1..10] //True
//Start = f7 [1,2,3] //True
//Start = f7 [2,3,4] //False
//Start = f7 [1,3,4,5] //False
//Start = f7 [1,2,3,4,6,7] //False
//Start = f7 [] //False




/*____________________________________________________________________________________________________________________________________________________*/



// 8. Write a function that removes consecutive duplicates in a list.

/////iterate throught each element and if you find two numbers equal consecutively, igonre numbers equal to them until you hit a number that is not equal to them. (DROPWHILE)
f8 :: [Int] -> [Int]
f8 [] = []
f8 [a] = [a]
f8 [a,b:rest]
| a == b = f8(dropWhile(\x -> x==a) rest)
= [a] ++ f8 [b:rest]



//Start = f8 [4,5,6,6,8,2,2,2,4,0,0,0,7,0,5,0,0,4] //[4,5,8,4,7,0,5,4]
//Start = f8 [1,0,0,2,0,3,3,0,6,7,0,7,7] //[1,2,0,0,6,7,0]
//Start = f8 [2,0,0,6,7,5,0,8,0,5,0,0,0] //[2,6,7,5,0,8,0,5] 







/*_________________________________________________________________________________________________________________________________________________________________*/




// 9. Write a function that takes a tuple of three lists and generates a list of triple tuples.
// The triple tuple is only generated if the i-th member of the first list multiplied by the
// i-th member of the second list equals the i-th member of the third list.
// e.g. for ([1,2,3,4,5],[2,4,6,8,10],[2,8,17,32,45]) the result is [(1,2,2),(2,4,8),(4,8,32)]

f9 ::([Int],[Int],[Int])->[(Int,Int,Int)]
f9 (a,b,c) = [(x,y,z)\\x<-a & y<-b & z <-c | x*y == z]

//Start = f9 ([2,2,2,2,2,2],[1,2,3,4,5,6,7,8],[2,4,6,6,10])//[(2,1,2),(2,2,4),(2,3,6),(2,5,10)]
//Start = f9 ([1,2,3,4,5],[2,4,6,8,10],[2,8,1,32,45])//[(1,2,2),(2,4,8),(4,8,32)]
//Start = f9 ([1,0,1,0,1,0],[3,4,5,6,8],[3,0,5,0,0])//[(1,3,3),(0,4,0),(1,5,5),(0,6,0)]




/*___________________________________________________________________________________________________________________________________________________________________*/




// 10. Write a function that checks if a number is a Mersenne Prime.
// A Mersenne Prime is a prime number that is 1 less than a power of 2. example: 7 = (2^3) - 1 = 8-1

////// YOU DON'T TAKE CLASSES TO OPEN YOUR MIND for NEW WAYS, YOU PRACTICE!!!!! ///////
f10 :: Int -> Bool
f10 num 
| num > 1 = or[num == (2^x - 1)\\ x<-[1..toInt(sqrt (toReal num))] | isPrime x]
= False

//Start = f10 2147483647 //True  ////FUN FACT: This is the eighth Mersenne Prime
//Start = f10 2305843009213693951 //True ///FUN FACT: This is the nineth Mersenne Prime
//Start = f10 618970019642690137449562111 ///RUN TIME (CLEAN IS SUCKED xD)
//Start = f10 7 //True
//Start = f10 1 //False
//Start = f10 (~235) //False
//Start = f10 0 //False




/*____________________________________________________ :) ______________________________________________________________________________________________________________*/
