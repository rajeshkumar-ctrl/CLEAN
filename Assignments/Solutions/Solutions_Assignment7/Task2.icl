moudle Task2
import StdEnv


/* 2. Define a Person record which contains name and height two fields,
with type of String and Real respectively. Write a function which takes a person
and a certain height, if the person is taller than 1.70, subtract their height by
1%.

*/

::Person1 = { name1 :: String, tall :: Real}
John::Person1
John={name1 = "John", tall= 1.78}
Mike::Person1
Mike={name1 = "Mike", tall= 1.58}
Lily::Person1
Lily={name1 = "Lily", tall= 1.85}


ChangeHeight :: Person1 -> Person1
ChangeHeight p
| p.tall > 1.70 = { name1 = p.name1, tall = p.tall - ((p.tall *1.0)/100.0)}
= p


//Start = ChangeHeight John // (Person1 "John" 1.7622)
//Start = ChangeHeight Mike // (Person1 "Mike" 1.58)
//Start = ChangeHeight Lily // (Person1 "Lily" 1.8315)

