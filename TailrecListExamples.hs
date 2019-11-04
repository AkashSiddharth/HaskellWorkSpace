-- $Id: TailrecListExamples.hs,v 1.1 2015/02/20 20:23:17 leavens Exp $
module TailrecListExamples where
import Prelude hiding (sum, length, elem)

-- The following functions are all coded tail-recursively
sum :: (Num a) => [a] -> a
sum lst = sum_iter lst 0
sum_iter [] acc = acc
sum_iter (x:xs) acc = sum_iter xs (x+acc)
    
length :: [a] -> Int
length lst = len_iter lst 0
len_iter [] acc = acc
len_iter (_:xs) acc = len_iter xs (1+acc)

average, average' :: (Fractional a) => [a] -> a
average lst = (sum lst) / (fromIntegral (length lst))
-- more directly computing the average, without using sum and length
average' lst = avg_iter lst 0 0
avg_iter [] tot cnt = tot / cnt
avg_iter (x:xs) tot cnt = avg_iter xs (x+tot) (1+cnt)

occurrences :: (Eq a) => a -> [a] -> Int
occurrences what lst = occ_iter lst 0
      where occ_iter [] acc = acc
            occ_iter (x:xs) acc =
                  occ_iter xs ((if x == what then 1 else 0) + acc)

elem,elem' :: (Eq a) => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys) = x == y || (elem x ys)
-- desugaring the above, to show how the above is really a tail call
elem' _ [] = False
elem' x (y:ys) = if x == y
                 then True
                 else (elem' x ys)

