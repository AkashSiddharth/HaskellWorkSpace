{- Problem: Write a function
                foldWindowPlan :: ((String,Int,Int) -> r) -> ([r] -> r) -> ([r] -> r) -> WindowPlan -> r
            that abstracts from all the WindowPlan examples we have seen (such as those earlier in this homework
            and similar WindowLayout examples on the course examples page). For each type r, the function
            foldWindowPlan takes 3 functions: wf, hf, and vf, which correspond to the three variants (Win, Horiz,
            and Vert) in the grammar for WindowPlan. In more detail:
                • wf, operates on a tuple of the information from a Win variant and returns a value of type r,
                • hf, takes a list of the results of mapping (foldWindowPlan wf hf vf) over the list in a Horiz
                  variant, and returns a value of type r,
                • vf, takes a list of the results of mapping (foldWindowPlan wf hf vf) over the list in a Vert
                  variant, and returns a value of type r.
-}

module FoldWindowPlan where
    import WindowPlan

    -- Function Signature
    foldWindowPlan :: ((String,Int,Int) -> r) -> ([r] -> r) -> ([r] -> r) -> WindowPlan -> r

    -- Case structure can also be used here
    foldWindowPlan wf hf vf (Win s w h) = wf (s, w, h)
    foldWindowPlan wf hf vf (Horiz wps) = hf (map (foldWindowPlan wf hf vf) wps)
    foldWindowPlan wf hf vf (Vert wps) = vf (map (foldWindowPlan wf hf vf) wps)