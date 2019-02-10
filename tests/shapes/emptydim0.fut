-- ==
-- input { empty([]i32) [[1]] } output { empty([]i32) [[1]] }
-- input { [[1]] empty([]i32) } output { [[1]] empty([]i32) }
-- input { [[1]] [[1,2]] } error: .
-- input { [[1,2]] [[1]] } error: .

let main [n] (xs: [][n]i32) (ys: [][n]i32) = (xs, ys)
