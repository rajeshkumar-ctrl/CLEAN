module PracforMid
import StdEnv


//I added some explanations to it.

// 1. Generate the first 10 positive elements of a list in which a number is multiple of 5 but is not multiple of 10.

glist :: [Int]
glist = take 10 ([5*x \\ x <- [1..] | not (isEven x) ] )

glist1 = take 10 [x \\ x<-[1..]|(x rem 5==0) &&(x rem 10 <>0)]

glist2 = [5*x\\x<-[1,3..19]]

//Start = fun1

// 2. Compute the product of the elements of the sublists of a list (you MUST use map)

multiply :: [Int] -> Int
multiply [] = 1
multiply list =  foldr (*) 1 list

plist :: [[Int]] -> [Int]
plist list = map multiply list

plist1 x = map prod x

//Start = plist [[1, 2, 3], [3, 4], [5, 7, 8, 9], []]
prod :: [Int] -> Int
prod [] = 1
prod list = foldr (*) 1 list 

//[[1, 2, 3], [3, 4], [5, 7, 8, 9], []]
//Start = prod [1,2,3] // 6 


//Start = map prod [[1, 2, 3], [3, 4], [5, 7, 8, 9], []]

// 3. Given a list of tuples make a tuple of 2 lists like:

clist :: [(Int, Int)] -> ([Int], [Int])
clist list = unzip list
/*
h1 [] = []
h1 [(x,y):xs] =   [x] ++ h1 xs
d1 [] = []
d1 [(x,y):xs] =   [y] ++ d1 xs
clist1 x = (h1 x,d1 x)
*/
clist2 l = ([fst x \\ x<-l], [snd x \\ x<-l])

//Start = clist2 [(1, 2), (3,1), (8, 4), (5, 7), (8, 9)] // ([1,3,8,5,8],[2,1,4,7,9])


aux1 :: [(Int,Int)] -> [Int]
aux1 [] = []
aux1 [(x,y):xs] = [x] ++ aux1 xs
//Start = aux1 [(1, 2), (3,1), (8, 4), (5, 7), (8, 9)] //[1,3,8,5,8]
aux2 :: [(Int,Int)] -> [Int]
aux2 []=[]
aux2 [(x,y):xs] = [y]++ aux2 xs
//Start = aux2 [(1, 2), (3,1), (8, 4), (5, 7), (8, 9)]//[2,1,4,7,9]
linker :: [(Int,Int)] -> ([Int],[Int])
linker list = (aux1 list,aux2 list)
//Start = linker [(1, 2), (3,1), (8, 4), (5, 7), (8, 9)]//([1,3,8,5,8],[2,1,4,7,9])

// 4. Insert x as second element in every sublist of a list.
// if the sublist was empty then x will be the only element in the new sublist.
// [[1,2], [3,4,5], [6,5,9,7], [], [8]] 100 -> [[1,100,2], [3,100,4,5], [6,100,5,9,7], [100], [8,100]]

insert1 :: Int [Int] -> [Int]
insert1 x [] = [x]
insert1 x [n:ns] = [n,x : ns]

//Start = insert1 100 [1,2,3,4,5]  //[1,100,2,3,4,5]

f1 :: [[Int]] Int  -> [[Int]]
f1 list x = map (insert1 x) list 


//Start = f1 [[1,2], [3,4,5], [6,5,9,7], [], [8]] 100// [[1,100,2],[3,100,4,5],[6,100,5,9,7],[100],[8,100]]

insertx :: [Int] Int -> [Int]
insertx [] x = [x]
insertx [n:ns] x = [n,x:ns]

xlist :: [[Int]] Int -> [[Int]]
xlist list x = map (\n = insertx n x) list
//another way..
ins :: Int [Int] -> [Int]
ins n [] = [n]
ins n x = [hd x] ++ [n] ++ (drop 1 x)
//Start = ins 100 [1,2,3,4] // [1,100,2,3,4]

xlist2 :: [[Int]] Int -> [[Int]]
xlist2 x n = map (ins n) x

//Start = xlist2  [[1,2], [3,4,5], [6,5,9,7], [], [8]] 100 //[[1,100,2],[3,100,4,5],[6,100,5,9,7],[100],[8,100]] 

xlist1 [] n = [n]
xlist1 [x] n = [x,n]
xlist1 [x,y:xs] n = [x,n,y: xs]

xlist3 [] _ = []
xlist3 [x:xs] n = [xlist1 x n : xlist3 xs n]

//Start = xlist [[1,2], [3,4,5], [6,5,9,7], [], [8]] 100

// 5. Generate pairs like in the following:
// [[1,2,3], [4,5], [6,7,8], []] -> [(1,6),(2,9),(3,21),(4,0)]

//most optimum solution.
sol1 :: [[Int]]-> [(Int,Int)]
sol1 list = zip([1..], map sum list)

//Start = sol1 [[1,2,3], [4,5], [6,7,8], []] //[(1,6),(2,9),(3,21),(4,0)]
//Start = sol1 [[]] // [(1,0)]

fpair` :: [[Int]] Int -> [(Int, Int)]
fpair` [] y = []
fpair` [x:xs] y = [(y+1, (foldr (+) 0 x)): fpair` xs (y+1)]

fpair :: [[Int]] -> [(Int, Int)]
fpair list = fpair` list 0

fpair1 x = zip([1..], map sum x)

fpair11 :: [Int] -> Int
fpair11 [] = 0
fpair11 [x:xs] = x + fpair11 xs
//Start = fpair11 [1,2,3] // 6
fpair22 :: [[Int]] -> [Int]
fpair22 [] = []
fpair22 [x:xs] = [fpair11 x : fpair22 xs]
//Start = fpair22 [[1,2,3],[9,9,9],[1,0,1]]//[6,27,2]

fpair2 x = [(x,y) \\ x<-[1..] & y<-fpair22 x]
//with list comprehensions
fpair3 l = [(x,y) \\ x<-[1..] & y<-(map sum l)]

	
//Start = fpair1 [[1,2,3],[4,5],[6,7,8],[]]


// 6. Extract the second element of each sublist of a list (if there is no second element, ignore that sublist)
// e.g. [[1,2,3], [3,4,5,6], [], [5,7,8], [1], [8,9]]-> [2,4,7,9]

//This is my solution.
extrafun :: [Int] -> [Int]
extrafun [] = []
extrafun [x] = []
extrafun [x,y:xs] = [y]
//Start = extrafun [1,2,3,4]// [2]
//Start = flatten (map extrafun [[1,2,3], [3,4,5,6], [], [5,7,8], [1], [8,9]]) //[2,4,7,9]

//another one 
qlist :: [[Int]] -> [Int]
qlist [] = []
qlist [x:xs] 
| length x >1 = [ last (take 2 x) : qlist xs]
= qlist xs
//Start = take 2 [1,2,3]//[1,2]
//Start = last (take 2 [1,2,3]) // 2 
//Start = qlist [[1,2,3], [3,4,5,6], [], [5,7,8], [1], [8,9]]//[2,4,7,9]


//another way
cd :: [[Int]]-> [[Int]] // 
cd [] = []
cd [x:xs] 
|x==[] = cd xs
|length x ==1 = cd xs
=[x:cd xs] 
//Start = cd [[1,2,3], [3,4,5,6], [], [5,7,8], [1], [8,9]] //[[1,2,3],[3,4,5,6],[5,7,8],[8,9]] it removes all empty and one-element lists.

qlist11 :: [Int] -> Int // an extraction funciton
qlist11 [x,y:xs] = y 
//Start = qlist11 [1,2,3]// 2

qlist2 :: [[Int]] -> [Int] // the call function ; aka the linker between cd and qlist11
qlist2 [] = []
qlist2 [x:xs] =map qlist11 (cd [x:xs]) 
//Start = qlist2 [[1,2,3], [3,4,5,6], [], [5,7,8], [1], [8,9]]//[2,4,7,9]
//Start = map qlist11 [[1,2,3], [3,4,5,6], [], [5,7,8], [1], [8,9]]// why not just do this ? -> [2,4,Run time error bec of [] and [1] conditions
qlist3 :: [[Int]] -> [Int]
qlist3 l = map second ll //applies second to the list acquired after filtering
where ll = [x \\ x <-l | length x >=2] // any sublist inside the list has to be longer or equal to 2

//Start = qlist3 [[1,2,3], [3,4,5,6], [], [5,7,8], [1], [8,9]]// [2,4,7,9]

second :: [Int] -> Int // this one extracts the 2nd element from any list hence the sublists after getting mapped.
second x = x!!1

//Start = qlist [[1,2,3], [3,4,5,6], [], [5,7,8], [1], [8,9]]

// 7. Check if a list contains 2 equal elements one after the other
// (it can be anywhere in the list)
// for [1,2,3,3,3,2,4,5] is True for [1 .. 5] is False
//my solution
myfun :: [Int] -> Bool
myfun [] = False
myfun [x] = False
myfun [x,y:xs]
| x==y = True
= myfun [y: xs]
//Start = myfun [1,2,2,3,4,5]//true

dlist :: [Int] -> Bool
dlist [] = False
dlist [x] = False
dlist [x,y:xs]
| x==y = True
= dlist [y:xs]


check :: (Int,Int) -> Bool
check (x,y) = (x==y)
//Start = check (1,2) // false

dlist1 :: [Int] -> Bool
dlist1 x =  foldr (||) False (map check (zip (x, tl x)))

//Start = zip [1,2,3] ['abc'] //Output : cannot unify demanded type with offered type false example in the slides.
//Start = zip ([1,2,3],['abc']) //[(1,'a'),(2,'b'),(3,'c')]
//Start = zip ([1,2,3],[3,4,5]) // [(1,3),(2,4),(3,5)]
//Start = map check (zip([1,2,2,3,4,5],tl [1,2,2,3,4,5] )) // [False,True,False,False,False]
//Start = dlist1 [1,2,3,4,3,2,4,8,5,5]// true

// 8. Generate the following list
// [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],[1,2,3,4,5,6],[1,2,3,4,5,6,7],[1,2,3,4,5,6,7,8],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9,10]]

//Easiest way without building a function
//Start = [[x..y] \\ x<-[1],y<-[1..10]] // [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],[1,2,3,4,5,6],[1,2,3,4,5,6,7],[1,2,3,4,5,6,7,8],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9,10]]
// this bascially means form lists from [1..1] then [1..2] then [1..3] and so on, y changes it's value everytime 1 runs out which means each round once. then it puts all this in a list.

nlist :: Int -> [[Int]]
nlist n = [x \\ y <- [1..n], x<-[[1..y]]]
//x before the \\ is the output of the lsit comprehension. y iterates first because it is before the comma then x iterates until whatever value y is 
// also x  concatenates the other x lists together as it the expression starts again from 1 when a new value of y is set. and the x before \\ is the one that's being concatenated each time. 
//Start = nlist 10 

nlist1 :: Int -> [[Int]]
nlist1 n = take 10 [[x..y] \\ x<-[1..n],y<-[1..n]]

nlist2 n = [ [x\\x<-[1..y]] \\y<-[1..n]]
//same idea of nlist but written in a different way.
//same but the order is different and then we have to reverse it at the end or when calling the Start.
f :: Int -> [[Int]]
f n 
| n > 0 = [[1..n] : f (n-1)]
= []

nlist3 n = reverse (f n)


//Start = nlist 10
