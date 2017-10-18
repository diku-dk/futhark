-- Explicit shape quantification in lambda.
-- ==
-- input { 2 3 } output { [[3,4,5], [3,4,5]] }

let main (n: i32) (m: i32) =
  map (\[y] (r: [y]i32) -> map (+y) r) (replicate n (iota m))
