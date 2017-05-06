-- iota can be mapped.
-- ==
-- input { [2,2] } output { [[0,1],[0,1]] }
-- input { [2,1] } error: .

let main(ns: []i32) = map iota ns
