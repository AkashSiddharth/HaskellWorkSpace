-- Problem : Write a polymorphic funtion to rotate the number of elemets from
-- the input list

module Rotate where
import Number

rotate :: Number -> (a,a,a,a,a) -> (a,a,a,a,a)

rotate Zero (q, w, e, r, t) = (q, w, e, r, t)
rotate One (q, w, e, r, t) = (t, q, w, e, r)
rotate Two (q, w, e, r, t) = (r, t, q, w, e)
rotate Three (q, w, e, r, t) = (e, r, t, q, w)
rotate Four (q, w, e, r, t) = (w, e, r, t, q)

-- End of code