-- Shape-bound variables used inside a local function, but where the
-- array itself is not used.

let f(n: i32) = replicate n 0

let main (lower_bounds: [n]f64) =
  let rs = f n
  let init_i (rs: [n]i32) = map (\j -> lower_bounds[j]) (iota n)
  in init_i rs
