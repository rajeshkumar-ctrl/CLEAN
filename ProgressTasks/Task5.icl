module Task5
import StdEnv


rangeFilter :: (Int, Int) [(String,Int)] -> [String]

rangeFilter tuple list = [fst(x)\\ x <-list | snd(x) >= fst(tuple) && snd(x) <= snd(tuple)]


//Start = rangeFilter (10, 15) [("A",12),("B",3),("C",5),("E",14),("F",16)] // ["A","E"]
//Start = rangeFilter (3, 13) [("A",12),("B",3),("C",5),("E",14),("F",16)] // ["A", "B", "C"]
//Start = rangeFilter (5, 7) [] // []
//Start = rangeFilter (15, 3) [("A",12),("B",3),("C",5),("E",14),("F",16)] // []
//Start= rangeFilter (0, 2) [("A",12),("B",3),("C",5),("E",14),("F",16)] // []
