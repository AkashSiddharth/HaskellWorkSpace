{- Problem: Write the function
                filterInside :: (a -> Bool) -> [[a]] -> [[a]]
            that for some type a takes a predicate pred of type a -> Bool, and a list of lists of type a, lls, and
            returns a list of type [[a]] that consists of the elements of each element inside each list in lls, that
            satisfies pred (i.e., for which pred applied to that element returns True).
-}

module FilterInside where
    filterInside :: (a -> Bool) -> [[a]] -> [[a]]

    filterInside pred lls = [filter pred lst | lst <- lls]
