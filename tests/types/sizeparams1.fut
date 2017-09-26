-- Size-parameterised type in parameter.
-- ==
-- input { empty(i32) } output { 0 }
-- input { [1,2,3] } output { 3 }

type ints [n] = [n]i32

let main [n] (_: ints [n]) : i32 = n
