-- Problem : Write functions to operate on Binary Relations

module BinaryRelationOps where 
    import BinaryRelation

    -- returns a list of all the keys of the relation
    first :: (BinaryRelation a b) -> [a]
    first b_rel = [ fst pair | pair <- b_rel ]

    --  returns a list of all the values of the relation
    second :: (BinaryRelation a b) -> [b]
    second b_rel = [ snd pair | pair <- b_rel ]

    -- takes a predicate and a binary relation and returns a list of all the tuples
    -- in the relation that satisfythe predicate (in their original order).
    select :: ((a,b) ->Bool) -> (BinaryRelation a b) -> (BinaryRelation a b)
    select predicate b_rel = [ pair | pair <- b_rel, predicate pair ]

    -- takes two binary relation and returns their relational composition, that
    -- is the list of pairs(a, c)suchthat there is some pair(a, b)in the first
    -- argument binary relation and a pair(b, c)in the secondrelation argument.
    compose ::Eq b => (BinaryRelation a b) -> (BinaryRelation b c)-> (BinaryRelation a c)
    compose b_rel1 b_rel2 = [ (fst pair1, snd pair2) | pair1 <- b_rel1, pair2 <- b_rel2, snd pair1 == fst pair2 ]

-- End of code