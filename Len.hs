-- $Id: Len.hs,v 1.2 2019/09/26 19:55:36 leavens Exp $
module Len where

-- fully recursive version
lenFullyRecursive :: [a] -> Integer
lenFullyRecursive [] = 0
lenFullyRecursive (_:es) = 1 + (lenFullyRecursive es)

-- tail recursive version
len :: [a] -> Integer
len as = len_iter as 0
len_iter :: [a] -> Integer -> Integer
len_iter [] acc = acc
len_iter (_:es) acc = len_iter es (1+acc)
