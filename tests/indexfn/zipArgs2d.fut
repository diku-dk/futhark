def f [n][m] (xss: [n][m]i64): {[n][m]i64 | \_ -> true} = map (\xs -> map (\x -> x+1) xs) xss

def main p q: {[p][q]i64 | \_ -> true} =
  let X = map (\i -> map (\j -> i + j) (iota q)) (iota p)
  in f X
