module midone
import StdEnv 


//Define a function which finds the maximum of two numbers
maximum :: Int Int -> Int
maximum a b
| a > b = a
= b

//Start = maximum 3 4 // 4



//Given a list of Reals.
//Write a code which will add 1 to every real number from the list which is less than 10

//Recursive Function 
subst2 :: [Real] -> [Real]
subst2 [] = []
subst2 [a:b]
| a < 10.0 = [a+1.0 : subst2 b]
= [a: subst2 b]
//Start=subst2 [1.6,12.4,5.4,12.4] //[2.6,12.4,6.4,12.4]



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



//Given a list of Int.
//Write a function which will calculate the sum of the numbers up to the first number greater than 10*/
sumTillGreater::[Int]->Int
sumTillGreater list = sum(takeWhile(\x -> x <=10) list)
//Start=sumTillGreater [1,4,10,12]//15
//Start=sumTillGreater [1,2,3,4,5,6,7]//28
//Start=sumTillGreater []//0
//Start=sumTillGreater [10,11]//10






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




/*

Write a function, that takes a list of functions, and a list of
tuples (Int, Int) where the first Int indicates which function to
use and the second Int acts as a parameter and returns a list of
the results.
  
For example: Router [isEven,isOdd] [(1,2),(2,4),(1,57)] = [True, False, False]
*/

Router :: [(a->b)] [(Int,a)] -> [b]
Router [] _ = []
Router listOne [] = []
Router listOne [(a,b):rest] 
= [(f b)] ++ Router listOne rest    
where
	 f=last(take a listOne)

//Start = Router [isEven,isOdd] [(1,2),(2,4),(1,57)] //[True, False, False]
//Start = Router [((+)1),((*)2),((^)2),((rem) 100)] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[9,46,32,1337,8]
//Start = Router [(\x = [1..x]),(\x = [n\\n<-[1..x]|x rem n ==0]),(\x = [x,x*2..x*10])] [(2,36),(1,13),(3,5),(2,128),(3,1)]  //[[1,2,3,4,6,9,12,18,36],[1,2,3,4,5,6,7,8,9,10,11,12,13],[5,10,15,20,25,30,35,40,45,50],[1,2,4,8,16,32,64,128],[1,2,3,4,5,6,7,8,9,10]]
//Start = Router [] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[]








/*

Write a function that takes a list of integers and returns a list of
result integers based on how many integers were in the parameter list.
For 1 integer 'a', it will return that integer modulus 2. (a rem 2)
For 2 integers 'a','b' , it will return a list of all integers from the first to the second. [a..b]
For 3 integers 'a','b','c' , it will return (a*(b^c))
For 4 integers 'a','b','c','d', it will return a list of the sum of 'a' and 'b' and the sum of 'c' and 'd'.
*/

///Using Gaurds   
createlist :: Int Int -> [Int]  
createlist a b = [a..b]  

powerfunc :: Int Int -> Int
powerfunc _ 0 = 1
powerfunc f s = f * powerfunc f (s-1) 


Listing :: [Int] -> [Int]
Listing [] = []
Listing list 
| length list == 1 = [(hd list) rem 2]
| length list == 2 = createlist (hd list) (last list)
| length list == 3 = [(hd list) * (powerfunc (hd(tl list)) (last list))]
| length list == 4 = [hd list + hd (tl list),sum (tl ( tl list))] 


///Using Patterns Maching 
listing02 :: [Int] -> [Int]
listing02 [] = []
listing02 [a] = [a rem 2]
listing02 [a,b]  = [a..b]
listing02 [a,b,c] = [a*(b^c)]
listing02 [a,b,c,d] = [a+b,c+d]


//Start = listing02 [5] //[1]
//Start = listing02 [4,10] //[4,5,6,7,8,9,10]
//Start = listing02 [3,5,2] //[75]
//Start = listing02 [13,29,1030,307] //[42,1337]
//Start = listing02 [] //[]




/*
Write a function that checks if a list of numbers is odd,even,odd,even...
For exmaple: SeqCheck [1,2,3,4,6] = False because 4 is even, but 6 is not odd.
*/


///Naive Approach 
SeqCheck :: [Int] -> Bool
SeqCheck list 
| list == [] || isEven(hd list) = False
= and[isOdd(x+y)\\x<-list & y <- (tl list)]

///Evan's Approach 
sqC :: [Int] -> Bool
sqC list = and[isEven(x+y)\\x <- list & y <- [1..]]


//Start = sqC [1..10] //True
//Start = sqC [1,2,3] //True
//Start = sqC [2,3,4] //False
//Start = SeqCheck [1,3,4,5] //False
//Start = SeqCheck [1,2,3,4,6,7] //False
//Start = SeqCheck [] //False











/*

Write a function that checks if each elements in the list appear even times.
For example, checkEven [1,1,2,2,2,2,3,5,3,5] = True
*/
  
///first Approach   
checkOccrences :: Int [Int] -> Int
checkOccrences num list = length[x\\x<-list | x == num]

checkEven :: [Int] -> Bool
checkEven [] = False
checkEven list = and[isEven (checkOccrences x list)\\x<-list]



///Using Filter utility 
checkEven01 :: [Int]-> Bool
checkEven01 [] = False
checkEven01 list = and[isEven (length(filter (\e -> e == x) list))\\x<-list]



//Start = checkEven01 [1,1,2,2,2,2,3,5,3,5] // True
//Start = checkEven [1,1,2,2,1] // False
//Start = checkEven [] //False





/*

Write a function that takes two vectors, represented as lists, and returns their dot product.
The dot product of two vectors can be computed as:
< xa, xb, xc, ...> * < ya, yb, yc, ...> = (xa*ya) + (xb*yb) + (xc*yc) + ...
For example: DotProd [4,6,3] [6,3,7] = 24+18+21 = 63
*/

DotProd :: [Int] [Int] -> Int
DotProd l1 l2 = sum[x*y\\ x<- l1 & y<-l2]
//Start = DotProd [4,6,3] [6,3,7] //63
//Start = DotProd [6,3,7] [4,6,3] //63
//Start = DotProd [5,2,6,8,3] [5,-8,5,-3,-5] //0





/*
Given a list of characters, split it into a tuple in which the first part only contains digits ('0'..'9'),
the second part contains the rest. 
*/


///ASCII values for the digits are between 47 and 58 .... So, we can covert a char into ASCII value and check if it's in that range and if it is, we put it in the first list of the output tuple. 
isDigit :: Char -> Bool
isDigit char 
| toInt char < 58 = True 
= False

TwoLists :: [Char] -> ([Char], [Char])
TwoLists [] = ([],[])
TwoLists list = ([x\\x<-list | isDigit x ],[x\\x<-list | not (isDigit x)])


//TwoLists [a:b] = [ (x,y) \\ x <- list | toInt x < 58 , y <- list | toInt y >= 58]// ++ [x\\x<- list | toInt x > 58]
//Start = [(toInt x)\\ x <- ['0','1','2','3','4','5','6','7','8','9']] //(48--57)
//Start = [(toInt x)\\ x<- ['a'..'z']] //(97 -- 122)


//Start = TwoLists  ['1', 'a', '2', 'b', '3'] // (['1','2','3'],['a','b'])
//Start = TwoLists [] // ([],[])










/*
Given a list of lists, for each list, extract the first, middle and last element.
*/

Points3 :: [[Int]] -> [(Int, Int, Int)]
Points3 [[]] = []
Points3 listOfLists = [(hd list,last(take (((length list)/2)+1) list),last list)\\list<-listOfLists]


//Start = Points3 [[1..9], [2..6], [3..11], [1..10]] // [(1,5,9),(2,4,6),(3,7,11),(1,6,10)]
//Start = Points3 [[]] //[]






/*

Find the 'unique' right triangle in the list eg. (3,4,5) and (4,3,5) are the same triangle. 
only one will appear in the answer list [(3,4,5),(4,3,5)] -> [(3,4,5)]
*/



////I know it's not an affective solution!!! //////

isRightAngled :: (Int, Int, Int) -> Bool
isRightAngled (a,b,c)
| a > b && a > c = a*a == b*b + c*c 
| b > a && b > c = b*b == a*a + c*c
| c > a && c > b = c*c == a*a + b*b
= False

CrazyEqualityOfTuples :: (Int, Int, Int) (Int, Int, Int) -> Bool
CrazyEqualityOfTuples (a,b,c) (x,y,z)
| (abs a == abs x || abs a == abs y || abs a == abs z) && ( abs b == abs x || abs b == abs y || abs b == abs z) && ( abs c == abs x || abs c == abs y || abs c == abs z) = True
= False

remove :: (Int, Int, Int)[(Int, Int, Int)] -> [(Int, Int, Int)]
remove _ [] = []
remove x [a:b]
| CrazyEqualityOfTuples x a = remove x b
= [a:remove x b]

rmDups :: [(Int, Int, Int)] -> [(Int, Int, Int)]
rmDups [] = [] 
rmDups [firstTup : restTupes ] = [firstTup : rmDups (remove firstTup restTupes)]

f8::[(Int,Int,Int)]->[(Int,Int,Int)]
f8 listofTuples = rmDups[tuple\\tuple <- listofTuples | isRightAngled tuple]
  

//f8 listofTuples = [(a,b,c)\\(a,b,c)<- listofTuples | Rtri (a,b,c) && not(//checkUnique (a,b,c) )]
//Start = f8 [(3,4,5),(4,5,6),(4,5,3),(6,8,10),(10,5,8),(-3,4,5)] //[(3,4,5),(6,8,10)]
//Start = f8 [(1,1,1),(5,4,3),(3,4,5),(0,0,0)] //[(5,4,3)]




/*
Use foldr to check if the square root of each integer in a list are all integers. 
*/

f9::[Int] ->Bool
f9 [] = True
f9 list = and[toReal(toInt(sqrt(toReal x))) == sqrt(toReal x) \\ x <- list ] ///with list Comprehension 

f90 :: [Int] -> Bool
f90 list = foldr (\x y = y && toReal(toInt(sqrt(toReal x))) == sqrt(toReal x)) True list //foldr  

///Start = toReal(toInt(sqrt(toReal 2))) == sqrt(toReal 2)
//Start = f9 [] //True
//Start = f90 [4,16,9] //True
//Start = f9 [1,8] //False







/*
Insert sum of elements as last element in every sublist of a list. 
*/

insert :: Int [Int] -> [Int]
insert x list = list ++ [x]

addSum :: [[Int]] -> [[Int]]
addSum listOflists= [insert (sum list) list\\list <- listOflists]


//Start = addSum [[1,2], [3,4,5], [6,5,9,7], [], [8]] //[[1,2,3],[3,4,5,12],[6,5,9,7,27],[0],[8,8]]
