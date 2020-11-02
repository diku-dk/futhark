-- replicate can be mapped.
-- ==
-- input { 2i64 [true,false] } output { [[true,true],[false,false]] }

let main (n: i64) (xs: []bool) = map (replicate n) xs
