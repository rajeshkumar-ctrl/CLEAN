module third
import StdEnv

// Write a function that takes Int argument and checks if this number is a palindrome.

// Palindrome is a number that is the same when we read from left to right or from right to left.

makeList :: Int -> [Int]
makeList 0 = []
makeList n 
| n < 10 = [n]
= [n rem 10 : makeList(n/10)] 

is_palindrome :: Int -> Bool
is_palindrome num 
| num < 0 = False
= makeList num == reverse (makeList num) 

// Start = is_palindrome 0 // True

// Start = is_palindrome 55 // True

// Start = is_palindrome 49594 // True

// Start = is_palindrome 1337 // False

// Start = is_palindrome ~57975 // False // negative numbers are not palindromes.