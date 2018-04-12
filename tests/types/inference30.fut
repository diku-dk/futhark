-- Field projection inference for a lambda.
-- ==
-- input { 1 } output { [1] }

let main (x: i32) = map (\r -> r.l) [{l=x}]
