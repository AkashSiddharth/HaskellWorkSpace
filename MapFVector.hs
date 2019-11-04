{- Problem: Write the function
                mapFVector :: (a -> b) -> (FVector a) -> (FVector b)
            which for any types a and b takes a function, fun, of type (a -> b)
            and a (FVector a) value, fv, and returns an (FVector b) value, whose
            value at index i is the result of applying fun to (at fv i).
-}

module MapFVector where
    import FVector

    -- Function Signature
    mapFVector :: (a -> b) -> (FVector a) -> (FVector b)

    mapFVector fun (FV rule size) = (FV (fun . rule) size)