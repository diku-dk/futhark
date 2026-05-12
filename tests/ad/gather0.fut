-- ==
-- tags { autodiff }
-- entry: fwd_J rev_J
-- input { [4.0,3.0,2.0,1.0] [0i64,1i64,2i64,3i64] }
-- output { [[1.0, 0.0, 0.0, 0.0],
--           [0.0, 1.0, 0.0, 0.0],
--           [0.0, 0.0, 1.0, 0.0],
--           [0.0, 0.0, 0.0, 1.0]]
--        }
-- input { [4.0,3.0,2.0,1.0] [0i64,0i64,3i64,3i64] }
-- output { [[1.0, 0.0, 0.0, 0.0],
--           [1.0, 0.0, 0.0, 0.0],
--           [0.0, 0.0, 0.0, 1.0],
--           [0.0, 0.0, 0.0, 1.0]]
--        }

def gather xs is = map (\(i: i64) -> xs[i]) is

entry fwd_J [n] [m] (xs: [n]f64) (is: [m]i64) =
  transpose (tabulate n (\j -> jvp (`gather` is) xs (replicate n 0 with [j] = 1)))

entry rev_J [n] [m] (xs: [n]f64) (is: [m]i64) =
  tabulate m (\j -> vjp (`gather` is) xs (replicate m 0 with [j] = 1))
