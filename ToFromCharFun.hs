module ToFromCharFun where
    import Data.Char
    toCharFun :: (Int -> Int) -> (Char -> Char)
    fromCharFun :: (Char -> Char) -> (Int -> Int)
    toCharFun f x = chr (f (ord x))
    fromCharFun f x = ord (f (chr x))