-- Problem: Write a function to substitute texts in a sms to shorthand typing
{- Replacement Key:
    • you is replaced by u,
    • are is replaced by r,
    • your is replaced by ur,
    • the three words by the way are replaced by the word btw,
    • the three words for your information is replaced by the word fyi,
    • boyfriend is replaced by bf,
    • girlfriend is replaced by gf,
    • the three words be right back are replaced by the word brb,
    • the three words laughing out loud are replaced by the word lol,
    • the two words see you are replaced by the word cya,
    • the two words I will are replaced by the word I’ll,
    • the word to is replaced by the word 2, and
    • great is replaced by gr8.
-}

module Txt2sms where
    import Prelude hiding (Word)
    type Word = String

    sub_text = [("you", "u"), ("are", "r"), ("your", "ur"),("boyfriend", "bf"),
                ("girlfriend", "gf"), ("to", "2"), ("great", "gr8")]
    

    -- Structure definition
    txt2sms :: [Word] -> [Word]

    -- Base Definition
    txt2sms [] = []

    txt2sms ("by":"the":"way":rest_sms) = "btw" : txt2sms rest_sms
    txt2sms ("for":"your":"information":rest_sms) = "fyi" : txt2sms rest_sms
    txt2sms ("be":"right":"back":rest_sms) = "brb" : txt2sms rest_sms
    txt2sms ("laughing":"out":"loud":rest_sms) = "lol" : txt2sms rest_sms
    txt2sms ("see":"you":rest_sms) = "cya" : txt2sms rest_sms
    txt2sms ("I":"will":rest_sms) = "I'll" : txt2sms rest_sms
    txt2sms (wrd:rest_sms) = [ if key == wrd then sub else wrd | (key, sub) <- sub_text ] : txt2sms rest_sms
    
        
        
       -- slang_dictionary(wrd) : txt2sms rest_sms
    {- where
        slang_dictionary wrd  = [ if key == wrd then sub else wrd | (key, sub) <- sub_text ] -}
-- End of code