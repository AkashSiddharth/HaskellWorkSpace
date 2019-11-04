{- Problem: using tail recursion, write a polymorphic function 
findIndex :: (Eq a) => a -> [a] -> Integer
that takes an element of some Eq type, a, sought, and a finite list, lst, and returns the 0-based index of
the first occurrence of sought in lst. -}

module FindIndex where

    -- Structure definition
    findIndex :: (Eq a) => a -> [a] -> Integer

    findIndex sought lst = findIndex_helper sought lst 0

    -- Base Index
    findIndex_helper _ [] _ = -1

    findIndex_helper sought (hd:rest_list) index_counter = if (sought == hd)
                                                           then index_counter
                                                           else findIndex_helper sought (rest_list) (index_counter + 1)
