-- Actually check against the function return type, too.
-- ==
-- error: 10

let main (xs: []i32) (ys: []i32) : [10]i32 = xs ++ ys
