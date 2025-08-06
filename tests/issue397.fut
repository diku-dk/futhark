-- ==
-- input {0} error:

def predict (a: [10]f64) : i64 =
  let (m, i) =
    reduce (\(a, i) (b, j) -> if a > b then (a, i) else (b, j))
           (a[9], 9)
           (zip (a[:8]) (iota 9 :> [8]i64))
  in i

def main (x: i32) : i64 =
  predict [0.2, 0.3, 0.1, 0.5, 0.6, 0.2, 0.3, 0.1, 0.7, 0.1]
