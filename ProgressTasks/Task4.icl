module Task4
import StdEnv

insert_at_pos [[Int]]-> [Int]

insert_at_pos []= []

insert_at_pos [x,y:xs] = [[1]++ x \\ x <-[x:xs]] ++ insert_at_pos [[2]++ (drop 1 y) \\ y <-[x,y:xs]]

//Start = insert_at_pos [[5],[6,9],[],[7,1,3,6]]  //[[1,5],[6,2,9],[3],[7,1,3,4,6]]

// Start = insert_at_pos [[0,3],[],[6,7,7,7]] //[[1,0,3],[2],[6,7,3,7,7]]
