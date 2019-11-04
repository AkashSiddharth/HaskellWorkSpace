{- Problem: Write two functions
                toCharFun :: (Int -> Int) -> (Char -> Char)
                fromCharFun :: (Char -> Char) -> (Int -> Int)
            The first function, toCharFun takes a function of type Int -> Int, and returns a function that
            operates on Chars. The second function, fromCharFun takes a function of type Char -> Char, and
            returns a function that operates on Ints.
-}

module ToFromCharFun where
    toCharFun :: (Int -> Int) -> (Char -> Char)

    toCharFun func chr = toEnum(func(fromEnum(chr)))

    fromCharFun :: (Char -> Char) -> (Int -> Int)
    
    fromCharFun func inc = fromEnum(func(toEnum(inc)))