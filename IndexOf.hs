-- $Id: IndexOf.hs,v 1.1 2019/09/20 15:19:53 leavens Exp leavens $
module IndexOf where

-- Both of the functions in this module are tail recursive.

-- indexOf is tail recrusive because it calls a tail recursive function
indexOf :: (Eq a) => a -> [a] -> Integer
indexOf sought lst = indexOf' sought lst 0

-- indexOf' is the auxilliary function, coded in a tail recursive style
indexOf' :: (Eq a) => a -> [a] -> Integer -> Integer
indexOf' sought [] acc = (-1)
indexOf' sought (a:as) acc =
          if sought == a
          then acc
          else indexOf' sought as (1+acc)
