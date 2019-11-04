{- Problem: write a general rating function
                rate :: (Ord a) => [a] -> [(Int, a)]
            which for any type a that is an instance of the Ord class, takes a list of elements of type a, things,
            and returns a list of pairs of Ints and a elements. The result is sorted (in non-decreasing order) on 
            the a elements of things, and the Int in each pair is the rating of the element in the pair.
-}

module Rate where
    import Data.List

    rate :: (Ord a) => [a] -> [(Int, a)]

    -- Base Case
    rate []  = []

    rate lst = rateHelper (sort lst) (head lst) 1 1

    rateHelper :: (Ord a) => [a] -> a -> Int -> Int -> [(Int, a)]

    rateHelper [] _ _ _ = []
    rateHelper (curr:rest) prev rating index = (nr, curr) : (rateHelper rest curr nr (index+1))
                                                where nr = if curr == prev then rating
                                                           else index