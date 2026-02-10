-- ==
-- tags { no_webgpu }

def main [n] [m] (xs: *[n][m]f64) =
  #[unsafe]
  let (a, b) = unzip (tabulate m (\i -> (f64.sqrt (f64.i64 i), f64.cos (f64.i64 i))))
  let xs[0] = a
  let xs[1] = b
  in (a, xs)
