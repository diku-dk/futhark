-- Reverse-mode scatter where the scatter is unrolled into a sequence of
-- update_accs because the input is a singleton array. This used to produce a
-- zero adjoint because diffUpdateAcc did not propagate the accumulator adjoint
-- (with overwritten positions zeroed) to the input accumulator.
-- ==
-- tags { autodiff }
-- input { [1.0] }
-- output { [1.0] }

def f [n] (xs: [n]f64) =
  let is = iota n ++ replicate n (-1)
  let res = scatter (replicate n 0.0) is (xs ++ xs)
  in f64.sum res

entry main (xs: [1]f64) =
  vjp f xs 1.0
