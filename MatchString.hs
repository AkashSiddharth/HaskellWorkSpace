module MatchString where
    matchString str s
                | isPrefixOf str s = Just(drop (length str) s)
                | otherwise = Nothing
