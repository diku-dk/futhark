def loc (n : i64) (i : i64) (r : i64) : i64 =
  if 0 <= i + r && i + r < n
  then i + r
  else if i + r < 0
       then n + i + r
       else i + r - n

-- TODO add permutation post condition
def rotate [n] 't (r: {i64 | \j -> 0 <= j && j < n}) (a: [n]t) : [n]t =
  map (\i -> a[loc n i r]) (iota n)
