module task2
import StdEnv


pt2 :: [Int]-> [Int]
pt2 [] = [] 
pt2 [x,y] = [x,y]
pt2 [x,y,z] = [z]
pt2 [x,y,z: sx] =  [[x,y,z: sx]!!2] ++ pt2 (drop 3 [x:sx])

Start = pt2 [1,2,3,5,8,9]
