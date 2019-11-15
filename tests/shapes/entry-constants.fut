-- Dimension declarations on entry points can refer to constants.
-- ==
-- input { [1,2,3] } output { [0,1] }
-- compiled input { [1,2] } error: Error
-- compiled input { [1,3,2] } error: Error

let three: i32 = 3
let two: i32 = 2

let main(a: [three]i32): [two]i32 = iota a[1] :> [two]i32
