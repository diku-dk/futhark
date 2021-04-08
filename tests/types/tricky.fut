-- ==
-- input { 4 }
-- output { [1i32, 1i32, 1i32] [4i32, 4i32, 4i32] }

type^ t [n] = ([n]i32, i32 -> [n]i32)

let v : t [] = let three = 3 in (replicate three 1, \i -> replicate three i)

let main x = (v.0, v.1 x)
