-- $Id: Reverse.hs,v 1.1 2013/02/07 19:17:57 leavens Exp $
module Reverse where
import Prelude hiding (reverse)

reverse:: [a] -> [a]
-- fully recrusive implementation
reverse [] = []
reverse (x:xs) = (reverse xs) ++ [x]

rev :: [a] -> [a]
-- tail recrusive implementation
rev ls = rev_iter (ls, [])
    where rev_iter ([], acc) = acc
          rev_iter (x:xs, acc) = rev_iter (xs, x:acc)
