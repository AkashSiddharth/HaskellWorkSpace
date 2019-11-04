-- $Id: ListOperations.hs,v 1.1 2013/02/07 19:17:57 leavens Exp leavens $
module ListOperations where
import Prelude hiding ((++),all)

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

all :: (a -> Bool) -> [a] -> Bool
all p [] = True
all p (x:xs) = (p x) && (all p xs)

-- fully recursive version of length
len :: [a] -> Integer
len [] = 0
len (_:xs) = 1 + (len xs)

len' :: [a] -> Integer
-- tail recursive version of length
len' ls = len_iter (ls, 0)
    where len_iter ([], n) = n
          len_iter ((_:xs), n) = len_iter (xs, n+1)
