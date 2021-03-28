module progtask
import StdEnv

pow :: Int Int -> Int
pow a b
| b == 0 = 1
= a * pow a (b-1)



Start = pow 2 3
