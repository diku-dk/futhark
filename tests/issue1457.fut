def main [n] [m] (xs: *[n][m]f32) =
  #[unsafe]
  let (a, b) = unzip (tabulate m (\i -> (f32.sqrt (f32.i64 i), f32.cos (f32.i64 i))))
  let xs[0] = a
  let xs[1] = b
  in (a, xs)
