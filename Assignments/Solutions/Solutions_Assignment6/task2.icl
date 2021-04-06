
/*2. Given the list of points and a distance. Each point is represented with tuple, containing X and Y coordinates
in 2D plane. Return how many pair of pointrs are there so that, distance between them is equal to the given value.
*/
pointDistance :: [(Int,Int)] Int -> Int
pointDistance list x = length (countero list x)


counter :: [(Int,Int)] Int -> [Int]
counter [] n = []
counter [x] n = []
counter [x,y:xs] n
| point (x,y) == n*n = [point(x,y)] ++ counter [y:xs] n 
= counter [y:xs] n

point :: ((Int,Int),(Int,Int)) -> Int
point ((a,b),(c,d)) = ((c-b)*(c-b))+((d-a)*(d-a)) 

// Start = pointDistance [(1,1), (4,5), (8,8), (10,3)] 5 // 2
// Start = pointDistance [(1,1), (4,5), (8,8), (10,3)] 2 // 0
// Start = pointDistance [(3,4), (3,8), (7,8)] 4 // 2
// Start = pointDistance [] 3 // 0
// Start = pointDistance [(1,1)] 2 //
