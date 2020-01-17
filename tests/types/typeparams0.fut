-- A simple case of a parametric type.
-- ==
-- input { 2 } output { [0,1] }

type^ vector 't = []t

let main(n: i32): vector i32 = iota n
