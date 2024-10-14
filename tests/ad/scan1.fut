-- Scan with addition.
-- addition special case
-- ==
-- entry: fwd_J rev_J
-- input { [1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32] }
-- output { [[1.0f32, 0.0f32, 0.0f32, 0.0f32, 0.0f32],
--           [1.0f32, 1.0f32, 0.0f32, 0.0f32, 0.0f32],
--           [1.0f32, 1.0f32, 1.0f32, 0.0f32, 0.0f32],
--           [1.0f32, 1.0f32, 1.0f32, 1.0f32, 0.0f32],
--           [1.0f32, 1.0f32, 1.0f32, 1.0f32, 1.0f32]]
--        }

entry fwd_J [n] (a: [n]f32) =
  tabulate n (\i -> jvp (scan (+) 0) a (replicate n 0 with [i] = 1))
  |> transpose

entry rev_J [n] (a: [n]f32) =
  tabulate n (\i -> vjp (scan (+) 0) a (replicate n 0 with [i] = 1))
