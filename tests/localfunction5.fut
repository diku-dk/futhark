-- Shape-bound variables used inside a local function, but where the
-- array itself is not used.

def f (n: i64) = replicate n 0

def main [n] (lower_bounds: [n]f64) =
  let rs = f n
  let init_i [n] (rs: [n]i32) = map (\j -> lower_bounds[j]) (iota n)
  in init_i rs
