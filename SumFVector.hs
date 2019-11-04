{- Problem: write the function
                sumFVector :: (Num t) => (FVector t) -> t
            that for any numeric type t, takes an (FVector t) value, fv, and returns the
            sum of that vector’s elements. If the vector is empty (has no elements), then
            zero is returned.
-}

module SumFVector where
    import FVector

    -- Function Signature
    sumFVector :: (Num t) => (FVector t) -> t

    sumFVector (FV r s)
        | s > 0 = ((at (FV r s) (s-1)) + sumFVector (FV r (s-1)))
        | otherwise = 0