{- Problem: Write a function
                scaleFVector :: (Num t) => t -> (FVector t) -> (FVector t)
            that for any numeric type t takes a value of type t, x, and a (FVector t) value, fv,
            and returns a FVector such that the ith element of the result is x times the ith 
            element of fv.
-}

module ScaleFVector where
    import FVector

    -- Function Signature
    scaleFVector :: (Num t) => t -> (FVector t) -> (FVector t)

    scaleFVector n (FV rule size) = (FV (\i -> n * (rule i)) size)


