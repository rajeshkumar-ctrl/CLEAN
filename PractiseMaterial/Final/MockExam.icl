module MockExam
import StdEnv



// Solve as many functions as you can. Each exercise is of 10%, to pass min. 40% is necessary.
// marks: 40%-2,60%-3,80%-4,100%-5. 

//1.
//Define a tree type and use the followings for testing your solution.

tree1 = Node 10 (Node 7 (Node 3 Leaf Leaf) (Node 15 Leaf Leaf)) (Node 5 Leaf (Node 10 Leaf Leaf))
tree2 = Node 9 (Node 1 (Node 0 (Node 7 Leaf Leaf) Leaf) (Node 15 Leaf Leaf)) (Node 4 (Node 4561 Leaf Leaf) (Node 8 (Node 1663 Leaf Leaf) Leaf))
unitTree = Node 1337 Leaf Leaf
noTree = Leaf

:: Tree a = Node a (Tree a) (Tree a) | Leaf


//Write a function that takes a tree as a parameter and returns a list of nodes which have at least one prime child.
//An empty tree will return [].


isPrime :: Int -> Bool
isPrime x = and[x rem n <> 0\\n<- [2..(x-1)]]

getChildren :: (Tree Int) -> [Int]
getChildren Leaf = []
getChildren (Node x Leaf Leaf) = []
getChildren (Node _ (Node a _ _) Leaf ) = [a] 
getChildren (Node _ Leaf (Node b _ _) ) = [b] 
getChildren (Node _ (Node a _ _)(Node b _ _)) = [a,b] 

// Start = getChildren tree1

PrimeChildren :: (Tree Int) -> [Int]
PrimeChildren Leaf = []
PrimeChildren t
| a =  PrimeChildren l ++ [x] ++ PrimeChildren r
= PrimeChildren l ++ PrimeChildren r
where
    a = or(map isPrime (getChildren t))
    (Node x l r) = t 

// Start = PrimeChildren tree1 
// tree1 = Node 10 (Node 7 (Node 3 Leaf Leaf) (Node 15 Leaf Leaf)) (Node 5 Leaf (Node 10 Leaf Leaf))

//Start = primeChildren tree1 //[10,7]
//Start = primeChildren tree2 //[0,4,8]
//Start = primeChildren unitTree //[]
//Start = primeChildren noTree //[]




//2.
//Given a tuple of arrays, representing sets of integers, return a list containing the result of their symmetric-difference.
//The symmetric-difference between two sets is equivalent to the difference between their union and their intersection.



symmetricDiff :: ({Int},{Int}) -> [Int]
symmetricDiff (a1,a2) = [x\\x<-l1 | not(isMember x l2)] ++ [x\\x<-l2 | not(isMember x l1)]
where
    l1 = [e\\e<-:a1]
    l2 = [e\\e<-:a2]


// Start = symmetricDiff ({1,2,3,4},{3,4,5,6}) //[1,2,5,6]
// Start = symmetricDiff ({1,2,3,4},{-2,-4,13,0}) //[1,2,3,4,-2,-4,13,0]
//Start = symmetricDiff ({1,2,3,4},{1,2,3,4}) //[]
//Start = symmetricDiff ({1,2,3,4},{}) //[1,2,3,4]
//Start = symmetricDiff ({},{1,2,3,4}) //[1,2,3,4]
// Start = symmetricDiff ({},{}) //[]




//3.
//Define a Q type for rational numbers.
//Write a function that receives two fractions and calculates their division. Simplify the fraction before returning.
//In case the nominator is zero, set the denominator to zero as well.

:: Q = {nom :: Int, den :: Int}

simplify :: Q -> Q
simplify {nom=n,den=d}
| d == 0 = abort "denominator is 0"
| d < 0 =  {nom = ~n/g , den = ~d/g}
| otherwise = {nom = n/g , den = d/g}
where
	g = gcmd n d 

gcmd:: Int Int -> Int 
gcmd x y = gcdnat (abs x) (abs y)
where
	gcdnat x 0 = x 
	gcdnat x y = gcdnat y (x rem y)


mkQ :: Int Int -> Q 
mkQ n d = simplify {nom=n,den = d}

// Start = mkQ 36 -4
instance / Q 
where
    (/) x y = simplify{nom=x.nom *y.den, den =x.den* y.nom}

fracDivision :: Q Q -> Q
fracDivision a b = a/b

// Start = {nom=5, den=1} / {nom=6, den=5}
// Start = fracDivision {nom=10, den=2} {nom=3, den=4} //(Q 20 3)
// Start = fracDivision {nom=0, den=2} {nom=100, den=4} //{nom=0, den=1}
// Start = fracDivision {nom=15, den=2} {nom=3, den=12} //{nom=30, den=1}

//4.
//Write a function that will return the sum of a tree's nodes.
//Return the sum as a simplified Q.
//:: Q = {nom :: Int, den :: Int}

half = { nom=1, den=2 }
third = { nom=1, den=3 }
fourth = { nom=1, den=4 }
fifth = { nom=1, den=5 }
sixth = { nom=1, den=6 }
threehalf = { nom=3, den=2 }
twothird = { nom=2, den=3 }
ninefourth = { nom=9, den=4 }
threefifth = { nom=3, den=5 }

miniTree = Node fifth (Node sixth Leaf Leaf)(Node third (Node fourth Leaf Leaf) Leaf)			
smallTree = Node half (Node fourth Leaf Leaf) (Node ninefourth Leaf Leaf)
bigTree = Node half (Node fifth (Node sixth Leaf Leaf)(Node third (Node fourth Leaf Leaf) Leaf))(Node threehalf (Node threefifth Leaf (Node twothird Leaf Leaf))(Node ninefourth Leaf Leaf))
badTree = Node third (Node fourth Leaf Leaf)(Node ninefourth (Node sixth Leaf Leaf) Leaf)



instance + Q 
where
    (+) {nom =n1,den = d1} {nom = n2, den = d2} = {nom = n1*d2+ n2*d1, den = d1*d2}

// Start = fourth + fifth// (Q 9 20)

instance zero Q
where
    zero = {nom = 0,den = 1}



sumTree :: (Tree Q )-> Q
sumTree Leaf = zero
sumTree (Node x l r) = sumTree l + x + sumTree r

// Start = sumTree smallTree 



//5.
//Write a function that will check if a tree of Q is a Binary Search Tree.

instance == Q 
where
    (==) {nom =n1,den = d1} {nom = n2, den = d2} = n1*d2==n2*d1

instance < Q 
where
    (<) {nom =n1,den = d1} {nom = n2, den = d2} = n1*d2 <n2*d1 

checkTree :: (Tree Q) -> Bool
checkTree t = l == sort l
where
    l = TreetoList t


TreetoList :: (Tree a ) -> [a]
TreetoList Leaf = []
TreetoList (Node x l r) = TreetoList l ++ [x] ++ TreetoList r


// Start = checkTree bigTree //True
// Start = checkTree smallTree //True
// Start = checkTree badTree //False
:: Color = Red | Yellow | Green | Blue | Purple | Violet
:: ColorCombo = { color1 :: Color, color2 :: Color}

instance == Color
where
    == Red Red = True
    == Yellow Yellow = True
    == Green Green = True
    == Blue Blue = True
    == Purple Purple = True
    == Violet Violet = True
    == _ _ = False

colorList :: [Color]
colorList = [Red,Yellow,Green,Blue,Purple,Violet]

//6.
//Write a function that when given a color, returns its complement.
//That is:
//Red -> Blue, Yellow -> Purple, Green -> Violet, Blue -> Red, Purple -> Yellow, Violet -> Green

colorComp :: Color -> Color
colorComp c = newcolor
where
    index = hd[i\\ n <-colorList & i<-[0..] | n == c]
    newcolorIndex = (index + 3) rem 6
    newcolor = colorList!!newcolorIndex
// Start = 5 rem 6 


// Start = colorComp Red //Blue
//Start = colorComp Blue //Red
//Start = colorComp Green //Violet
// Start = colorComp Purple //Yellow

//7.
//Write a function that when given a Color, creates a list of possible color combos.
//Valid color combos can not have duplicate colors.
// :: ColorCombo = { color1 :: Color, color2 :: Color}

colorCombo :: Color -> [ColorCombo]
colorCombo c = colorCombolist
where
    index = hd[i\\n<-colorList & i<-[0..] | n==c]
    otherColors = [n\\n<-colorList & i<-[0..] | i<> index ]
    colorCombolist = [ { color1 = c , color2 = n}\\n<-otherColors]

// Start = colorCombo Red

/*3 Write a function which will take an array of Universities and return the University with the highest overall gpa (the average of the average of each student)*/
//highestGpa::{University}->String


::University={uniName::String,students::[Student],teachers::[Teacher]}
::Teacher={tname::String,subject::String}
::Student={studentName::String,age::Int,grades::{Int},favoriteTeacher::Teacher}

ELTE::University
ELTE={uniName="ELTE",students=[Marko,Nikola,Josh,Dame],teachers=[Mary,Peter,John]}

BMI::University
BMI={uniName="BMI",students=[Ana,Josh,Sofi,Nikola],teachers=[Viktor,John,Peter]}

EmptyUni::University
EmptyUni={uniName="Empty",students=[],teachers=[]}

Peter::Teacher
Peter={tname="Peter",subject="Functional"}
Viktor::Teacher
Viktor={tname="Viktor",subject="Math"}
Mary::Teacher
Mary={tname="Mary",subject="OOP"}
John::Teacher
John={tname="John",subject="Functional"}
Marko::Student
Marko={studentName="Marko",age=19,grades={4,4,4,5},favoriteTeacher= Mary}
Sofi::Student
Sofi={studentName="Sofi",age=22,grades={5,5,4,5,5},favoriteTeacher=John}
Dame::Student
Dame={studentName="Dame",age=21,grades={2,3,4,5},favoriteTeacher=Peter}
Ana::Student
Ana={studentName="Ana",age=18,grades={5,5,5,5},favoriteTeacher=Viktor}
Nikola::Student
Nikola={studentName="Nikola",age=19,grades={4,4,4,4,2},favoriteTeacher=Peter}
Nik::Student
Nik={studentName="Nik",age=20,grades={4,4,4,4,3},favoriteTeacher=Peter}
Nik2::Student
Nik2={studentName="Nik2",age=22,grades={4,4,4,4,5},favoriteTeacher=Peter}
Josh::Student
Josh={studentName="Josh",age=22,grades={4,5,5},favoriteTeacher=John}


Gpa :: Student -> Real
Gpa {grades=g} = Average
where
    list = [e\\e<-:g]
    Average = (toReal(sum list)) / (toReal (length list)) 

// Start = Gpa Marko // 4.25
// Start = Gpa Sofi // 4.8

UniGpa :: University -> [Real]
UniGpa {students = list} = map Gpa list

// Start = UniGpa ELTE //[4.25,3.6,4.66666666666667,3.5]

// Start = maxList (UniGpa ELTE) //4.66666666666667

// {ELTE,BMI,EmptyUni} [ELTE,BMI,EmptyUni]



highestGpa :: {University} -> String
highestGpa array
| len == 0 = "No universities given"
| name == "Empty" = "Empty"
| maxGpa == Gpauni = x.uniName
= highestGpa arrminus1
where
    list = [e\\e<-:array]
    maxGpa = maxList (flatten (map UniGpa list))
    Gpauni = (maxList (UniGpa x))
    x = (hd list) 
    xs = (tl list)
    arrminus1 = {e\\e<-xs}
    name = x.uniName
    len = (length list)



// Start=highestGpa {ELTE,BMI,EmptyUni}//"BMI"
// Start=highestGpa {ELTE,BMI} //"BMI
// Start=highestGpa {EmptyUni,EmptyUni}//"Empty"
// Start=highestGpa {ELTE} //"ELTE"
// Start=highestGpa {}//"No universities given"

/*2 Given a University, return an array of all the 
students or teachers names which are shorter than 6*/
// ::University={uniName::String,students::[Student],teachers::[Teacher]}

ForStudent:: Student -> Bool
ForStudent x = len < 6
where
    name = x.studentName
    nameList = [e\\e<-:name]
    len = length nameList

ForTeacher :: Teacher -> Bool
ForTeacher x = len < 6
where
    name = x.tname
    nameList = [e\\e<-:name]
    len = length nameList
// Start = ForTeacher Peter // True
// ::University={uniName::String,students::[Student],teachers::[Teacher]}
// ::Teacher={tname::String,subject::String}
// ::Student={studentName::String,age::Int,grades::{Int},favoriteTeacher::Teacher}

// Start = map ForTeacher [Viktor, Mary, Peter] // [False,True,True]
// Start = map ForStudent [Marko, Nikola] //[True,False]
FuncauxSTD :: [Student] -> [String]
FuncauxSTD [] = []
FuncauxSTD list
| ForStudent x  = [x.studentName] ++ FuncauxSTD xs
= FuncauxSTD xs
where
    x = hd list
    xs = tl list
// Start = FuncauxSTD  [Marko, Nikola,Sofi]
FuncauxTCR :: [Teacher] -> [String]
FuncauxTCR [] = []
FuncauxTCR list
| ForTeacher x  = [x.tname] ++ FuncauxTCR xs
= FuncauxTCR xs
where
    x = hd list
    xs = tl list
// Start = FuncauxTCR  [Peter, Viktor,Mary]

// shorterThan6::University->{String}
// shorterThan6 uni 
// | Empty = {}
// = array
// where
//     STDlist = uni.students
//     TCRlist = uni.teachers
//     x = hd uni.teachers
//     y = hd uni.students
//     namelistSTD  =  FuncauxSTD STDlist // [list of names]
//     namelistTCR = FuncauxTCR TCRlist //  [list of names]
//     combined =  namelistSTD ++ namelistTCR
//     array = {e\\e<-combined}
//     Empty = uni.uniName == "EmptyUni"

/* A more optimized solution*/
bettershorterThan6::University->{String}
bettershorterThan6 uni = {c\\c<-list}
where
	list=[x.studentName\\x<-uni.students | size (x.studentName)<6] ++ [x.tname\\x<-uni.teachers | size(x.tname)<6]
// Start=shorterThan6 BMI//{"Ana","Josh","Sofi","John","Peter"} done
// Start=shorterThan6 ELTE//{"Marko","Josh","Dame","Mary","Peter","John"} done 
// Start=shorterThan6 EmptyUni//{} done

/*4	Create a toString instance for Student such that for given student ex. Nikola={studentName="Nikola",age=19,grades={4,4,4,4,2},
favoriteTeacher=Peter} it gives "Nikola 3.6 Peter" where 3.6 is the student's gpa and Peter is the student's favorite teacher's name*/
//instance toString Student

instance toString Teacher
where
    toString teacher = teacher.tname

instance toString Student
where
    toString student = student.studentName +++ " " +++ toString (Gpa student) +++ " " +++  toString student.favoriteTeacher

// Start=toString Nikola//"Nikola  3.6  Peter"
// Start=toString Marko//"Marko  4.25  Mary"
// Start=toString Nik//"Nik  3.8  Peter"
// Start=toString Dame//"Dame  3.5  Peter"


    
/* 5
A good person is the person who never lies, so let's test this quote, we have list of people, each person has name
and list of the names of the people that he lies to, 
your task is to get the people who can say the truth to all the people in the given list 
list can be empty if all the people did lie.
Example : goodPeople [{fake_name = "Rafaat Ismail", peopleToLie = ["Adel"]},{fake_name = "Lucifier", peopleToLie = ["Rafaat Ismail"]}
Output : [{fake_name = "Rafaat Ismail", peopleToLie = ["Adel"]}] because Adel is not on the given list.
note : ofcourse we will consider fake names for this expirement so all the names here are fictional.
*/

::Person = {fake_name :: String, peopleToLie :: [String]}







//6. Given two arrays, return new array such that i-th element of it is maximum of i-th element of first and second arrays.
// So for example, when we calculate 5th element of result array, we look at 5th element of first and 5th element of second arrays
// And choose maximum of the two.
// You can assume that arrays have same length. 

// maxOfTwo :: {Int} {Int} -> {Int}
// maxOfTwo a1 a2 = result
// where
//     composed = removeDup([a\\a<-:a1 & b <-:a2|a>=b] ++ [b\\b<-:a2 & a<-:a1|a<=b])
//     result = {e\\e<-composed}


maxOfTwo :: {Int} {Int} -> {Int}
maxOfTwo x y = {s \\ s <-recursion (toList x) (toList y) }
where
    recursion :: [Int] [Int] -> [Int]
    recursion [] []= []
    recursion [i:is] [j:js]
    |i > j = [i] ++ recursion is js
    |i < j = [j] ++ recursion is js
    = [i] ++ recursion is js
    toList :: {Int}->[Int]
    toList a = [ z \\ z <-: a ]


// Start = maxOfTwo {} {} // {}
// Start = maxOfTwo {1} {5} // {5}
// Start = maxOfTwo {1,5,4} {2,3,6} // {2,5,6}
// Start = maxOfTwo {1,2,3,4,5} {1,2,3,4,5} // {1,2,3,4,5}



//7. You are given array of integers.
// Your function should return true if each value appears at least twice in the array, and it should return false
// if any element is distinct.


// f7 :: {Int} -> Bool
// f7 x = recursion (toList x)
// where
//     toList :: {Int}->[Int]
//     toList a = [ z \\ z <-: a ]
//     recursion :: [Int] -> Bool
//     recursion [x:xs]
//     | not(isMember x xs ) = False
//     | (isMember x xs ) && (recursion xs) = True
//     = False


f7 :: {Int} -> Bool
f7 x = result
where
    toList :: {Int}->[Int]
    toList a = [ z \\ z <-: a ]
    checkOnePlz :: Int [Int] -> Bool
    checkOnePlz x list = length(filter ((==) x) list) >= 2
    result = and(map (\elem = checkOnePlz elem listarr) listarr)
    where
        listarr = toList x
// Start = f7 {1,2,3,1,3,2,2,2} // True
// Start = f7 {1,2,3,4,3,2,1} // False
// Start = f7 {1,1,1,3,3,4,3,2,4,2} // True
