-- Reverse-mode scatter where the scatter is unrolled into a sequence of
-- update_accs because the input is a singleton array. This used to produce a
-- zero adjoint because diffUpdateAcc did not propagate the accumulator adjoint
-- (with overwritten positions zeroed) to the input accumulator.
-- ==
-- tags { autodiff }
-- input { [1.0f32] }
-- output { [1.0f32] }

def f [n] (xs: [n]f32) =
  let is = iota n ++ replicate n (-1)
  let res = scatter (replicate n 0.0) is (xs ++ xs)
  in f32.sum res

entry main (xs: [1]f32) =
  vjp f xs 1.0
