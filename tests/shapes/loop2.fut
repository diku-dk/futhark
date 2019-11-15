-- The initial values for merge parameters must have the right size.
-- ==
-- error: \[n\]i32

let main [m] (xs: [m]i32) (n: i32) =
  loop (ys: [n]i32) = xs for _i < 3i32 do
    replicate n (ys[0]+1)
