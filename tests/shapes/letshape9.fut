-- ==
-- input {} output { 1i64 2i64 }

def main =
  let [n] (_: [n]i32, f: [n]bool -> [n]bool) =
    (replicate 1 0, \(xs: [1]bool) -> xs)
  let [m] (f: [m]bool -> [m]bool, _: [m]i32) =
    (\(xs: [2]bool) -> xs, replicate 2 0)
  in (n, m)
