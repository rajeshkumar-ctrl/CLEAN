module Task3
import StdEnv


:: Tree a = Node a (Tree a) (Tree a ) | Leaf

treea = (Node Functional (Node Astronomy (Node Programming (Node Astronomy Leaf Leaf) (Node Thermo_Dynamics Leaf Leaf)) Leaf)
(Node Basic (Node Compilers (Node Relativity Leaf Leaf) (Node Astronomy (Node Analysis Leaf Leaf) Leaf)) Leaf))

treeb = (Node Functional (Node Astronomy (Node Programming (Node Astronomy Leaf Leaf) Leaf) Leaf)
(Node Relativity (Node Compilers (Node Numerical_Methods Leaf Leaf) (Node Astronomy (Node Analysis Leaf Leaf) Leaf)) Leaf))

treec = (Node Analysis Leaf (Node Programming Leaf (Node Astronomy Leaf (Node Basic Leaf (Node Compilers Leaf
(Node Thermo_Dynamics Leaf (Node Numerical_Methods Leaf (Node Functional Leaf Leaf))))))))

/*
3. Given a tree of Courses, give back all the CS Courses
whose both children are either Physics courses or Leaf-s.
*/

getphysics :: (Tree Course) -> [Course]

 

//Start = getphysics treea // [(Course "Programming" CS 5),(Course "Compilers" CS 4)]
//Start = getphysics treeb // [(Course "Functional" CS 5),(Course "Programming" CS 5)]
//Start = getphysics treec // [(Course "Programming" CS 5),(Course "Compilers" CS 4),(Course "Functional" CS 5)]
