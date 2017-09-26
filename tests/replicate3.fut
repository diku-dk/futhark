-- Reshape/replicate simplification test.
-- ==
-- structure { Reshape 1 }

let main [n] (b: [n]i32, m: i32) =
  let c = reshape (n*m) b
  let d = replicate 10 c
  in reshape (2,5,n*m) d
