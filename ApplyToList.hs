module ApplyToList where
    import BinaryRelation
    applyRel :: (Eq k) => k -> (BinaryRelation k v) -> [v]
    applyRel ky pairs = [ v | (key, v) <- pairs, ky == key]
    applyToList :: (Eq k) => [k] -> (BinaryRelation k v) -> [v]
    applyToList keys pairs = applyToList' (length keys) keys [] pairs
    applyToList' :: (Eq k) => Int -> [k] -> [v] -> (BinaryRelation k v) -> [v]
    applyToList' n [] lst pairs = lst
    applyToList' n (x:xs) lst pairs = (applyToList' (n-1) xs (lst ++ (applyRel x pairs)) pairs)