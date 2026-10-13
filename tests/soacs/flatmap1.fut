-- flatmap where the lambda produces an array whose contents depend on
-- the element.  The varying sizes force the result buffer to grow.  The value
-- result is computed from the irregular result.
-- ==
-- input { [0i64, 4i64, 1i64, 3i64] [0i32, 1i32, 2i32, 3i32] }
-- output { [0i64, 4i64, 1i64, 3i64]
--          [true, false, false, false, true, true, false, false]
--          [0i64, 0i64, 4i64, 5i64]
--          [10i32, 11i32, 12i32, 13i32, 20i32, 30i32, 31i32, 32i32]
--          [0i32, 46i32, 20i32, 93i32] }

def main [n] (ks: [n]i64) (xs: [n]i32) =
  flatmap (\(k, x) ->
             let r = map (\i -> x * 10 + i32.i64 i) (iota k)
             in (r, i32.sum r))
          (zip ks xs)
