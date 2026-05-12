-- ==
-- input { 10i64 }
-- output { 130i32 }

def main m =
  let f [n] m' v: ([m']i32, (os: [n]i32) -> i32) =
    ( replicate m' (v + i32.i64 n)
    , i32.sum
    )
  let (x, g) = f m 3
  in g x
