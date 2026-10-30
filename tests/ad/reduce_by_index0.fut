-- ==
-- tags { autodiff }
-- entry: f_jvp f_jmp
-- input { [0i64,1i64,2i64,3i64] [1f32,2f32,3f32,4f32] }
-- output { [[1f32,0f32,0f32,0f32],[0f32,1f32,0f32,0f32],[0f32,0f32,1f32,0f32],[0f32,0f32,0f32,1f32]] }
def f [n] (is: [n]i64) (vs: [n]f32) =
  hist (+) 0 4 is (map (+ 2) vs)

entry f_jvp [n] (is: [n]i64) (vs: [n]f32) =
  tabulate n (\i -> jvp (f is) vs (replicate n 0 with [i] = 1))
  |> transpose

entry f_jmp [n] (is: [n]i64) (vs: [n]f32) =
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in jmp (f is) vs seeds
  |> transpose
