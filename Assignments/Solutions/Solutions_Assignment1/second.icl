module second
import StdEnv


// Write a function that takes Int and checks if this number is prime or not.

// handle the case of negative numbers (negative numbers are not primes).

// 0 and 1 are not prime numbers.

is_prime :: Int -> Bool
is_prime num 
| num < 2 = False 
= and[num rem x <> 0\\x<-[2..num-1]]

//Start = is_prime 5  // True

//Start = is_prime 2 // False

// Start = is_prime 1 // False

// Start = is_prime 2 // True

// Start = is_prime 2017 // True