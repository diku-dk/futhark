-- Multiple uses of same size in parameterised type.
-- ==
-- input { empty(i32) empty(i32) } output { empty(i32) }
-- input { [1,2,3] [1,2,3] } output { [1,2,3,1,2,3] }
-- input { [1,2,3] [1,2,3,4] } error:

type ints [n] = [n]i32

let main(a: ints [#n], b: ints [#n]) = concat a b
