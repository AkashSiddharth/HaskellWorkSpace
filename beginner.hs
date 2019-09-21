doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

addOne x = (if x > 100 then x else x * 2) + 1

l = [1,3..20]

l1 = [x*2 | x <- [1..10]]
l2 = [x*2 | x <- [1..10], x*2 > 12]

l7 = [x | x <- [50..100], (mod x 7) == 3]

bBang xs = [if x < 10 then "Boom" else "Bang" | x <- xs, odd x] 


dismember lst = 
    let first = head lst
    in let rest = tail lst
     in (lst, first:rest)