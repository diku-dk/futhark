-- ==
-- input { [1,2,3] } output { 4 }

let apply 'a (f: a -> a) (x: a) = f x

let f [n] (xs: [n]i32) (x: i32) = n + x

let main (xs: []i32) = apply (f xs) 1
