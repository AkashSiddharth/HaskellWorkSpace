lucky :: (Integral n) => n -> String
lucky 7 = "You got a seven"
lucky a = "Try again"

factorial :: (Integral n) => n -> n
factorial 1 = 1
factorial n = n * factorial (n - 1)

vectorAdd :: (Num a) => (a, a ) -> (a, a) -> (a, a)
vectorAdd (a,c) (b,d) = (a+b, c+d)

header :: [a] -> a
header (x:_) = x
header [] = error "Can't call on an empty list"

listReader :: (Show a) => [a] -> String
listReader [] = "This is an empty list."
listReader (x:[]) = "The list has one element: " ++ show x
listReader (x:y:[]) = "The list has 2 elements: " ++ show x ++ " and " ++ show y
listReader (x:y:_) = "The list is long. The first 2 elements are: " ++ show x ++ " and " ++ show y

wordLength :: (Integral b) => [a] -> b
wordLength [] = 0
wordLength (_:xs) = 1 + wordLength xs