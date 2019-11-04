{- Problem: In Haskell, write a function 
                height :: WindowPlan -> Int
            that takes a WindowPlan, wp, and returns the total height of the window plan (in pixels). The height 
            is defined by cases as follows. The height of a WindowPlan of form (Win nm w h) is h. The height of 
            a WindowPlan of the form (Horiz [wp1, : : :, wpm]) is 0 if the list is empty, and otherwise is the
            maximum of the heights of wp1 through wpm (inclusive). The height of a WindowPlan of the form
            (Vert [wp1, : : :, wpm]) is the sum of the heights of wp1 through wpm (inclusive), which is 0 if 
            the list is empty. (You may assume that this sum is never greater than the largest Int.)
-}

module Height where
    import WindowPlan

    height :: WindowPlan -> Int

    -- Simple case
    height (Win _ _ h ) = h

    -- Case of Horiz (win_list)
    height (Horiz wp_list) = find_Max_Height [height wp | wp <- wp_list] 0

    -- Case of Vert (Win_list)
    height (Vert wp_list) = get_Height_Sum [height wp | wp <- wp_list] 0

    -- Tail Recursion for find_Max_Height
    find_Max_Height :: [Int] -> Int -> Int

    -- Base Case
    find_Max_Height [] max_height = max_height

    find_Max_Height (h:tl) temp_max = if (h > temp_max)
                                      then find_Max_Height tl h
                                      else find_Max_Height tl temp_max
    
    -- Tail Reursion for get_Height_Sum
    get_Height_Sum :: [Int] -> Int -> Int

    -- Base Case
    get_Height_Sum [] total_height = total_height

    get_Height_Sum (h:tl) acc = get_Height_Sum tl (acc + h)