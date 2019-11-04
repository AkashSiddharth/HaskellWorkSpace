{- Problem: Write the polymorphic function
                concatMap :: (a -> [b]) -> [a] -> [b]
            This function can be considered to be an abstraction of problems like deleteAll.
            An application such as (concatMap f ls) applies f to each element of ls, and 
            concatenates the results of those applications together (preserving the order).
-}

module ConcatMap where
    import Prelude hiding (concatMap)

    -- Function Signature
    concatMap :: (a -> [b]) -> [a] -> [b]
    
    concatMap f ls = foldr ((++).f) [] ls