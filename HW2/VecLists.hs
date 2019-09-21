-- Problem: Implement vector operation functions:scale,add, and sub

module VecLists where
    -- VecLists are represented by finite lists of coordinate values.
    type VecList = [Double]

    -- scale returns the VecList with each coordinate multiplied by the Double
    scale :: Double -> VecList -> VecList
    scale d_val v_list = [ d_val * vector | vector <- v_list ]

    -- add returns a VecList that is the pointwise sum of the two arguments
    -- The two arguments are assumed to have the same length.
    add :: VecList -> VecList -> VecList
    add v_list1 v_list2 = [ vector1 + vector2 | (vector1, vector2) <- zip v_list1 v_list2 ]

    -- dotprod returns the dot product of the VecList arguments
    -- The two arguments are assumed to have the same length.
    dotprod :: VecList -> VecList -> Double
    -- Base Case
    dotprod [] [] = 0.0
    dotprod (v1:v_list1) (v2:v_list2) = (v1 * v2) + dotprod v_list1 v_list2

-- End of code

