{- Problem: Using tail recursion, write a polymorphic function listMax :: (Ord a) => [a] -> a
            that takes a non-empty, finite list, lst, whose elements can be compared (hence the requirement in the
            type that a is an Ord instance), and returns a maximal element from lst. -}

module ListMax where

    -- Structure definition
    listMax :: (Ord a) => [a] -> a
    listMax_counter :: (Ord a) => [a] -> a -> a

    --Base Case
    listMax_counter [] tmp = tmp

    listMax_counter (hd:list_tail) tmp = listMax_counter list_tail (max hd tmp)

    listMax (hd:list_tail) = listMax_counter list_tail hd

-- End of code