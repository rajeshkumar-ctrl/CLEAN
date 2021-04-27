module Task2
import StdEnv
/*
2. Given a list of Courses that a student has taken,
find the credits they earned if
for each CS major course, he gets 3 more credits
*/

creds :: [Course] -> Int

 

//Start = creds [Compilers, Astronomy, Basic] // 16
//Start = creds [Thermo_Dynamics, Relativity, Numerical_Methods] // 14
//Start = creds [Compilers, Functional, Programming] //23
//Start = creds [] // 0

:: Tree a = Node a (Tree a) (Tree a ) | Leaf

treea = (Node Functional (Node Astronomy (Node Programming (Node Astronomy Leaf Leaf) (Node Thermo_Dynamics Leaf Leaf)) Leaf)
(Node Basic (Node Compilers (Node Relativity Leaf Leaf) (Node Astronomy (Node Analysis Leaf Leaf) Leaf)) Leaf))

treeb = (Node Functional (Node Astronomy (Node Programming (Node Astronomy Leaf Leaf) Leaf) Leaf)
(Node Relativity (Node Compilers (Node Numerical_Methods Leaf Leaf) (Node Astronomy (Node Analysis Leaf Leaf) Leaf)) Leaf))

treec = (Node Analysis Leaf (Node Programming Leaf (Node Astronomy Leaf (Node Basic Leaf (Node Compilers Leaf
(Node Thermo_Dynamics Leaf (Node Numerical_Methods Leaf (Node Functional Leaf Leaf))))))))
