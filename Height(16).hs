{- Problem: Use the foldWindowPlan function to implement:
                (a) height from problem 1
-}
module Height where
    import WindowPlan
    import FoldWindowPlan

    --Function Signature
    height :: WindowPlan -> Int

    height (Horiz []) = 0
    height (Vert []) = 0

    height window = foldWindowPlan (\(_, _, h) -> h) maximum sum window