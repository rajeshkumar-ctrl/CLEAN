module Task3
import StdEnv

Fib :: Int -> Int
Fib 0 = 1
Fib 1 = 1
Fib n = Fib (n-1) + Fib (n-2)


generateFibOdd :: [Int] -> [Int]
generateFibOdd list = [Fib x\\x<-list | (Fib x) rem 2 <> 0 ]

Start = generateFibOdd [ 3,7,11]
