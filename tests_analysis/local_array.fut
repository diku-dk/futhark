def main [n] (xs: [n]i32) =
  map (\x ->
    let xx = #[sequential] scan (+) 0 xs
    in #[unsafe] xx with [xx[x]] = 4243
  ) xs
