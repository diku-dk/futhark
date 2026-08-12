-- Reverse-mode AD of the program from fwd/acc0.fut, which accumulates with an
-- operator rather than overwriting. The second dataset writes to the same cell
-- twice, so that the incoming value of a cell that is later updated must retain
-- its sensitivity.
import "../accs/intrinsics"

-- ==
-- tags { autodiff }
-- entry: rev_map rev_vec
-- input { [0, 1, 2, 3] }
-- output {
-- [[2, 0, 0, 0],
--  [0, 2, 0, 0],
--  [0, 0, 2, 0],
--  [0, 0, 0, 2]]
-- }
-- input { [0, 0, 1, 1] }
-- output {
-- [[2, 1, 0, 0],
--  [0, 1, 1, 1],
--  [0, 0, 1, 0],
--  [0, 0, 0, 1]]
-- }

def f (acc: *acc ([]i32)) i = write acc i (i32.i64 i)

def primal [n] (xs: [n]i32) =
  let (xs': *[n]i32) = copy xs
  in reduce_by_index_stream xs' (+) 0 f (map i64.i32 (xs :> [n]i32))

def cotangents (n: i64) = tabulate n (\i -> replicate n 0i32 with [i] = 1)

entry rev_map [n] (xs: *[n]i32) = map (vjp primal xs) (cotangents n)

entry rev_vec [n] (xs: *[n]i32) = mjp primal xs (cotangents n)
