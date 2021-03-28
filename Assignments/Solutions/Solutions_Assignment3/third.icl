/* 3.
write a function that takes a list of lists(suppose that there are no empty sublists) and an integer,
do the following operations:
1) select the smallest value from each sublist
2) pick elements from the above resulting list which is smaller than the given integer and is an even number at the same time.
Assume that Real number is even if 'toInt' gives an even number.
*/

select :: [[Real]] Int -> [Real] 
select [] n = []
select listoflist n = filter (\x -> toInt(x) < n  && toInt(x) rem 2 == 0) [Min1 list\\list<-listoflist]

Min1 :: [Real] -> Real 
Min1 [x] = x  
Min1 [x:xs]
| x < hd xs = Min1([x] ++ (drop 1 xs))
= Min1(drop 1 [x:xs])

//Start=select [[3.2,4.9],[2.4,2.4,5.0],[2.2,3.3,4.4],[2.0],[1.3,3.8]] 3 // [2.4,2.2,2]
//Start=select [[5.0,2.5,7.7],[5.0,3.8,2.4],[5.6,7.9,10.76]] 4 // [2,5, 2.4]
//Start=select [[3.2,4.9],[3.4,12.4,5.0],[21.2,3.3,4.4],[2.0],[1.3,3.8,7.9]] 3 // [2]
//Start=select [] 1//[]