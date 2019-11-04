{- Problem: Write a function
                split :: String -> WindowPlan -> WindowPlan
            that takes a string, name, and a WindowPlan, wp, and returns a WindowPlan that is just like wp,
            except that for each window in wp whose name is (== to) name is changed to a Horiz window plan 
            with both windows having the same name and half the width of the previous window plan.
-}

module Split where
    import WindowPlan

    split :: String -> WindowPlan -> WindowPlan

    split key (Win name len hgt) = if (key == name)
                            then ((Horiz (let w = (Win name (len `div` 2) hgt) in [(w), (w)])))
                            else (Win name len hgt)
    split key (Horiz lst) = Horiz([split key e | e <- lst])
    split key (Vert lst) = Vert([split key e | e <- lst])
