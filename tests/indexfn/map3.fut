def all_ [n] p (xs: [n]i64) =
  let s = scan (+) 0 (map (\x -> if p x then 1 else 0) xs)
  in if n > 0 then s[n-1] == n else true

def f [n] (xs: [n]i64) (inds: {[n]i64 | all_ (>= 1)}) =
  map (\i -> xs[i-1]) inds
