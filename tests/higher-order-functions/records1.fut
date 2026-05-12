-- Implicit record field referring to a local first-order function.
-- ==
-- input { 5 10 12 } output { 37 }
-- input { 11 3 9 } output { 26 }

def main (k: i32) (m: i32) (n: i32) =
  let r =
    (let a = m
     let f (x: i32): i32 = x + a
     in {a, n = k, f})
  in r.f n + r.a + r.n
