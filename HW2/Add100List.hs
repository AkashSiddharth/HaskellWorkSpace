-- Problem : Write a function that takes a list of Integers and returns a list that is just like the argument but in which every element
-- is 100 greater than the corresponding element in the argument list.
module Add100List where
    -- List Comprehension
    add100_list_comp :: [Integer] -> [Integer]
    add100_list_comp list = [ num + 100 | num <- list ]

    -- List by recursion
    add100_list_rec :: [Integer] -> [Integer]
    add100_list_rec [] = []
    add100_list_rec (num:list)  = ((num + 100) : (add100_list_rec list))

    -- End of code
