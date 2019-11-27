-- ==
-- input { empty([0][1]i32) [[1]] } output { empty([0][1]i32) [[1]] }
-- input { [[1]] empty([0][1]i32) } output { [[1]] empty([0][1]i32) }
-- compiled input { [[1]] [[1,2]] } error: .
-- compiled input { [[1,2]] [[1]] } error: .

let main [n] (xs: [][n]i32) (ys: [][n]i32) = (xs, ys)
