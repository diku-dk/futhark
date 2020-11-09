-- It is legitimate for a loop to have an undefined size.
-- ==
-- input { 4 }
-- output { [0i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0i32] }

let main (n: i32) : []i32 =
  loop xs = replicate 1 0 for _i < n do
    xs ++ xs
