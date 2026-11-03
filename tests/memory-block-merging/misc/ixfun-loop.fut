-- A simple test for index-function generalization across a for loop
-- ==
-- input { [0i64, 1000i64, 42i64, 1001i64, 50000i64] }
-- output { 1249975000i64 }

def main [n] (a: [n]i64) : i64 =
  let b =
    loop b = iota (10)
    for i < n do
      let m = a[i]
      in iota (m)
  in reduce (+) 0 b
