module Assigthree
import StdEnv
/*
1. Compute the average of a list of float point numbers with foldr
*/

avg :: [Real]-> Real
avg [] = 0.0
avg list = ( foldr (+) 0.0 list ) / toReal(length (list))


//Start = avg [16.2, 17.8, 11.5]//15.1666666666667
//Start = avg [13.0, 40.9] // 26.95
//Start = avg [] // 0
