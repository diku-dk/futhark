def main [n] (xs: [n]i64) : [][]i64 =
  if n == 2
  then map (\_ -> xs) (iota n)
  else let largest = xs[0]
       in map (\_ -> iota largest) (iota (largest - 1))
