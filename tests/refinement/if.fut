def f [n] 't (xs: [n]i64) : {[n]i64 | \res-> permutationOf res (0...n-1)} =
  let y = map (\i -> if if xs[0] > 1337
                        then xs[i] > 0
                        else xs[i] > 1
                     then xs[i]
                     else xs[i]-1) (iota n)
  in y
