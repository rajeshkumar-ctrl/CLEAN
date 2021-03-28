module first
import StdEnv

// Write a function that will take a digit (Int)

// and return the respective word for it (String).

// For example input of 1 should output One; input of 0 should output Zero; input of 5 should output Five.

// Anything that is not the digit (0-9) should output "Not a digit"



digit_to_string :: Int -> String
digit_to_string a
| a == 0 = "Zero" 
| a == 1 = "One"
| a == 2 = "Two"
| a == 3 = "Three"
| a == 4 = "Four"
| a == 5 = "Five"
| a == 6 = "Six"
| a == 7 = "Seven"
| a == 8 = "Eight"
| a == 9 = "Nine"
= " Not a digit "

//Start = digit_to_string 2 //"Four"

//Start = digit_to_string 0 //"Zero"

//Start = digit_to_string 5 //"Five"

//Start = digit_to_string 10 //"Not a digit"

//Start = digit_to_string -1 //"Not a digit"

//Start = digit_to_string 42 //"Not a digit"