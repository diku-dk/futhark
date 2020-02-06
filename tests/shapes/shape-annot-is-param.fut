-- ==
-- input { 2 [1,2] }
-- output { [1,2] }
-- compiled input { 1 [1,2] }
-- error:

let f (n: i32) (xs: [n]i32): [n]i32 = xs

let main (n: i32) (xs: []i32) = f n xs
