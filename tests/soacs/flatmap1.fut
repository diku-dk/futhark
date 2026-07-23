-- flatmap where the lambda produces an array whose contents depend on
-- the element.  The varying sizes force the result buffer to grow.
-- ==
-- input { [0i64, 4i64, 1i64, 3i64] [0i32, 1i32, 2i32, 3i32] }
-- output { [10i32, 11i32, 12i32, 13i32, 20i32, 30i32, 31i32, 32i32] }

def main [n] (ks: [n]i64) (xs: [n]i32) =
  let (_, r) = flatmap (\k x -> map (\i -> x * 10 + i32.i64 i) (iota k)) ks xs
  in r
