-- Make sure loop parameters are not existential while checking the
-- loop.

let main [n][m] (A: [n][m]f32): [n][m]f32 =
  loop A for i < n do
    let irow = A[0]  -- Keep these
    let v1 = irow[i] -- separate.
    in  map (\k -> map (\j -> v1) (iota m)) (iota n)
