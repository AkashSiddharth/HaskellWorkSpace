module ListMax where
listMax :: (Ord a) => [a] -> a
listMax (a:[]) = a
listMax (a:b:as) = listMax' (a:b:as) (max a b) where
listMax' :: (Ord a) => [a] -> a -> a
listMax' (a:b:[]) _ = max a b
listMax' (a:b:as) acc = listMax' ((max a b):as) (max a b)