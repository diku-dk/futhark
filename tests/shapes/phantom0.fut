-- Maintenance of sizes that do not actually matter to the function result.
-- ==
-- input { [1] [2] } output { 1 }
-- input { [1] [2,3] } error:

let f [n] (_: [n]i32) (_: [n]i32) = n

let main (xs: []i32) (ys: []i32) = f xs ys
