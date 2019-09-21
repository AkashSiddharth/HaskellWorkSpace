-- Problem: Operations on binary relations

module ApplyToList where
    import BinaryRelation

    -- Return all values in a list (in-order of appearance) for which the key matches the argument 'k'
    applyRel :: (Eq k) => k -> (BinaryRelation k v) -> [v]
    applyRel key b_rel = [ v | (k,v) <- b_rel, key == k ]

    -- Return all values in a list (in-order of appearance) for which the key matches the argument list [k]
    applyToList :: (Eq k) => [k] -> (BinaryRelation k v) -> [v]
    -- Base Case
    applyToList [] b_rel = []
    applyToList key b_rel = applyRel (head key) b_rel ++ applyToList (tail key) b_rel
-- End of code