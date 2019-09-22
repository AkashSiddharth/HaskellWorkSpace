-- Problem:Ttakes a list of Integers, lst, and returns a list of Integers, except that each even element of lst is replaced by
-- the square of that element.

module SquareEvens where
    squareEvens :: [Integer] -> [Integer]
    squareEvens lst = [ if even num then num * num else num | num <- lst ]
-- End of code