{- Problem: Write the function
                addFVector :: (Num t) => (FVector t) -> (FVector t) -> (FVector t)
            that for any numeric type t, takes two (FVector t) values, fv1 and fv2 and if they have the same size,
            then it returns a new vector where the ith element of the result is the sum of the ith element of fv1 and
            the ith element of fv2. If fv1 and fv2 have different sizes, then an error should be signaled (by calling
            Haskell’s error function with a string argument explaining the error).
-}

module AddFVector where
    import FVector

    -- Function Signature
    addFVector :: (Num t) => (FVector t) -> (FVector t) -> (FVector t)

    addFVector (FV r1 s1) (FV r2 s2)
      | s1 == s2 = (FV (\i -> (r1 i) + (r2 i)) s1)
      | otherwise = error "Oops: FVectors size mismatch !!"