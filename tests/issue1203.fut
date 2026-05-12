def main [n] (xss: [][n]i64) (x: i64) =
  map (\xs ->
         #[sequential]
         loop (xs, x) for _i < n do
           let res = opaque (scan (*) 0 xs)
           let tmp = map2 (-) res xs
           let x' = reduce (+) x tmp
           in (res, x'))
      xss
