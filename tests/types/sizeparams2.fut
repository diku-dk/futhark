-- Multiple uses of same size in parameterised type.
-- ==
-- input { empty([0]i32) empty([0]i32) } output { empty([0]i32) }
-- input { [1,2,3] [1,2,3] } output { [1,2,3,1,2,3] }
-- input { [1,2,3] [1,2,3,4] } error:

type ints [n] = [n]i32

let main [n][m] (a: ints [n]) (b: ints [m]) = concat (a :> ints [n]) (b :> ints [n])
