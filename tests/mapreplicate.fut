-- replicate can be mapped.
-- ==
-- input { 2 [true,false] } output { [[true,true],[false,false]] }

let main (n: i32) (xs: []bool) = map (replicate n) xs
