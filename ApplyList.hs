module ApplyList where

    -- Function Signature
    applyList :: [(t -> t)] -> t -> [t]

    applyList funcList param = foldr (\param func param : acc) [] funcList