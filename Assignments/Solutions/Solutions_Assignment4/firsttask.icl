module task
import StdEnv


/*
1.Given two numbers L and R and the list of tuples, where the first element is an number
and the second is string. Return the list of strings from tuples where first element
is a number in interval [L,R].
*/

stringInRange :: Int Int [(Int,String)] -> [String]
stringInRange a b lists  = [snd(tuple) \\ tuple <-lists | b >= fst (tuple) && a <= fst(tuple)]

//Start = stringInRange 2 7 [(1,"A"), (2,"B"), (3,"C"), (11,"D"), (3,"E"), (5,"F")] // ["B","C","E","F"]
// Start = stringInRange 1 15 [(1,"A"), (2,"B"), (3,"C"), (11,"D"), (3,"E"), (5,"F")] // ["A","B","C","D","E","F"]
// Start = stringInRange 2 3 [(1,"A"), (2,"B"), (5,"F")] // ["B"]
// Start = stringInRange 3 4 [(1,"A"), (2,"B"), (5,"F")] // []
// Start = stringInRange 7 2 [(1,"A"), (2,"B"), (5,"F"), (3,"C"), (11,"D"), (3,"E"), (5,"F")] // []
