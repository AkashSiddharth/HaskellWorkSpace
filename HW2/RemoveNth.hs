-- Problem: Write module, where the function removes the Nth iteration of an 
-- element from the list, wothout using any library function

module RemoveNth where
    removeNth :: (Eq a)=> Int-> a -> [a] -> [a]

    -- Base Case
    removeNth iter item [] = []
    removeNth iteration item (n:rest_of_list)
        | iteration /= last_iteration && item == n = n : removeNth (decrease_iteration) item rest_of_list
        | iteration == last_iteration && item == n = removeNth (decrease_iteration) item rest_of_list
        | otherwise = n : removeNth iteration item rest_of_list
        where last_iteration = 1
              decrease_iteration = iteration - 1

-- End of code