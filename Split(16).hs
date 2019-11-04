{- Problem: Use the foldWindowPlan function to implement:
                (b) split from problem 2.
-}

module Split where
    import FoldWindowPlan
    import WindowPlan

    -- Function Signature
    split :: String -> WindowPlan -> WindowPlan

    split str wp = foldWindowPlan (\(i,j,k) -> if str == i then 
                                                (Horiz [(Win str (div j 2) k), (Win str (div j 2) k)])
                                               else (Win i j k)) Horiz Vert wp