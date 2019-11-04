module BinaryRelationOps where
import BinaryRelation
first :: (BinaryRelation a b) -> [a]
first [] = []
first ((k,v):rest) = k:first(rest)
second :: (BinaryRelation a b) -> [b]
second [] = []
second ((k,v):rest) = v:second(rest)
select :: ((a,b) -> Bool) -> (BinaryRelation a b) -> (BinaryRelation a b)
select pred [] = []
select pred ((k,v):rest) = if (pred (k,v)) 
                           then (k,v):select pred rest
                           else select pred rest
compose :: Eq b => (BinaryRelation a b) -> (BinaryRelation b c) -> (BinaryRelation a c)
compose list1 list2 = [(a,d) | (a,b) <- list1, (c,d) <- list2, b == c]