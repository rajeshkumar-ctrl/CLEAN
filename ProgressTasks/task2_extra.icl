module task2
import StdEnv


add  :: [Int] -> [Int]
add [] = []
add [x,y,z] = [x,y,z,1]
add [x,y] = [x,y]

add [x,y,z:sx] = [x,y,z] ++ [1] ++  add sx


Start = add [2,4,5,6,7,8,9,11,12]