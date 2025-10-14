def main (n: i64) =
  let a = replicate n (replicate n 1)
  in map (\(xs: []i32, i) -> copy xs with [0] = i32.i64 i) (zip a (iota n))
