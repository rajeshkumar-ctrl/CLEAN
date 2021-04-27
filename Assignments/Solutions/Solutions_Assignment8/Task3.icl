module Task3
import StdEnv

/* 3. Implement a function that interleaves three arrays. So for input arrays {1,2,3}, {4,5,6}
and {7,8,9} the function must return the array {1,4,7,2,5,8,3,6,9}. If an array is out of elements
we continue interleaving the remaining arrays.
Example: {1,2} {3,4,5,6} {7,8,9} -> {1,3,7,2,4,8,5,9,6}
*/



interleave :: {Int} {Int} {Int} -> {Int}
interleave x y z  = list_to_array(mapping (array_to_list x) (array_to_list y) (array_to_list z))

list_to_array :: [Int] -> {Int}
list_to_array list = {x\\x<-list}

array_to_list :: {Int} -> [Int]
array_to_list array = [x \\ x<-:array]

mapping::[Int] [Int][Int] -> [Int]
mapping [][][]=[]
mapping [] listtwo listthree = mapping2 listtwo listthree
mapping [] [] listthree = mapping3 listthree
mapping listone listtwo listthree = [hd listone] ++ [hd listtwo] ++ [hd listthree] ++ mapping (drop 1 listone) (drop 1 listtwo)(drop 1 listthree)

mapping2 :: [Int][Int]-> [Int]
mapping2 [] [] = []
mapping2 [x:xs][] =[x:xs]
mapping2 [][y:ys] = [y:ys]
mapping2 listone listtwo = [hd listone] ++ [hd listtwo] ++ mapping2 ( drop 1 listone) (drop 1 listtwo)

mapping3 :: [Int] -> [Int]
mapping3 [] =[]
mapping3 list = [y\\y<-list]

//Start = interleave {1,2,3} {4,5,6} {7,8,9} // {1,4,7,2,5,8,3,6,9}
//Start = interleave {1,2} {3,4,5,6} {7,8,9} // {1,3,7,2,4,8,5,9,6}
//Start = interleave {} {1,2,3} {4} // {1,4,2,3}
//Start = interleave {} {} {} // {}
//Start = interleave {1,2} {3,4,5} {6,7,8,9,10,11,12} // {1,3,6,2,4,7,5,8,9,10,11,12}
