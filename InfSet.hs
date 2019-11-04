{- Problem: Fill in the operations of the module InfSet, which are described informally as follows.
                1. The function
                    fromPred :: (a -> Bool) -> (Set a)
                takes a characteristic predicate, p and returns a set such that each value x (of type a) is in the set
                just when px is True.
                2. The function
                    unionSet :: Set a -> Set a -> Set a
                takes two sets, with characteristic predicates p and q, and returns a set such that each value x (of
                type a) is in the set just when either (px) or (qx) is true.
                3. The function
                    intersectSet :: Set a -> Set a -> Set a
                takes two sets, with characteristic predicates p and q, and returns a set such that each value x (of
                type a) is in the set just when both (px) and (qx) are true.
                4. The function
                    inSet :: a -> Set a -> Bool
                tells whether the first argument is a member of the second argument.
                5. The function
                    complementSet :: Set a -> Set a
                which returns a set that contains everything (of the appropriate type) not in the original set.
-}

module InfSet where
    -- define the type (Set a)
    data Set a = S (a -> Bool)

    -- Function Signature
    fromPred :: (a -> Bool) -> (Set a)
    fromPred pred = S pred

    -- Function Signature
    unionSet :: Set a -> Set a -> Set a
    unionSet (S pred1) (S pred2) = S (\a -> pred1 a || pred2 a)

    -- Function Signature
    intersectSet :: Set a -> Set a -> Set a
    intersectSet (S pred1) (S pred2) = S (\a -> pred1 a && pred2 a)

    -- Function Signature
    inSet :: a -> Set a -> Bool
    inSet x (S pred) = pred x

    -- Function Signature
    complementSet :: Set a -> Set a
    complementSet (S pred) = S (not.pred)