def index [n] 't (i: i64) (xs: [n]i64) : {i64 | \res-> permutationOf res (0...n-1)} =
  let y = xs[i]
  in y
