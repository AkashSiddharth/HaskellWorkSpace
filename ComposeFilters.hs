{- Problem: Write a function
                composeFilters :: [(Maybe a -> Maybe a)] -> (Maybe a -> Maybe a)
            that takes a list of filters, [f1,f2,: : :,fn] (for n >= 0) and returns
            the filter f1  f2  ...  fn, which is their composition.
-}

module ComposeFilters where

    -- Function Signature
    composeFilters :: [(Maybe a ->Maybe a)] -> (Maybe a -> Maybe a)

    composeFilters inFunc outItem = foldr (\x -> x) outItem inFunc

