-- ==
-- input { [1i64,2i64,3i64] } output { 4i64 }

let apply 'a (f: a -> a) (x: a) = f x

let f [n] (xs: [n]i64) (x: i64) = n + x

let main (xs: []i64) = apply (f xs) 1
