module AtwoTtwo
import StdEnv



// 2. Given a list of integers, find the prime numbers and compute the sum of them.
// Return 0 for empty lists or if there are no primes.



sum_of_prime :: [Int] -> Int
sum_of_prime [] = 0
sum_of_prime [x : xs] 
| isPrime x == True = x + sum_of_prime xs 
= sum_of_prime xs

isPrime :: Int -> Bool
isPrime x 
| x < 2 = False 
= and[x rem a <> 0\\a<-[2..x-1]]

Start = sum_of_prime [13, 3, 76, 17] // 33
//Start = sum_of_prime [14, 22, 45, 56] // 0
//Start = sum_of_prime [13, 27] // 13
//Start = sum_of_prime [] // 0