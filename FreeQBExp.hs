{- Problem: Your task is to write a function
                freeQBExp :: QBExp > (Set String)
            that takes a QBExp, qbe, and returns a set containing just the strings that occur as a free variable
            reference in qbe.
-}

module FreeQBExp where
    import QBExp

    freeQBExp :: QBExp -> (Set String)

    freeQBExp (Varref e) = [e]
    freeQBExp (Not qbe) = freeQBExp qbe
    freeQBExp (qbe1 `And` qbe2) = deleteDupes (freeQBExp qbe1 ++ freeQBExp qbe2) []
    
    freeQBExp (Forall key qbe) = [e | e <- (freeQBExp qbe), e /= key]

    deleteDupes :: [String] -> [String] -> [String]

    --A tail recursive method to delete duplicates
    deleteDupes [] lst = lst

    deleteDupes (x:xs) lst = deleteDupes [e | e <- xs, x /= e] (lst ++ [x])