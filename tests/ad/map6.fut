-- #1878
-- ==
-- tags { autodiff no_webgpu }
-- entry: fwd_J rev_J fwd_vec_J rev_vec_J
-- input { [1.0f32,2.0f32,3.0f32,4.0f32,5.0f32,6.0f32,7.0f32,8.0f32] }
-- output { [[0.0f32, 2.0f32, 3.0f32, 4.0f32],
--           [0.0f32, 0.0f32, 1.0f32, 1.0f32],
--           [0.0f32, 0.0f32, 0.0f32, 1.0f32],
--           [0.0f32, 0.0f32, 0.0f32, 0.0f32],
--           [-4.0f32, -6.0f32, -7.0f32, -8.0f32],
--           [0.0f32, 0.0f32, -1.0f32, -1.0f32],
--           [0.0f32, 0.0f32, 0.0f32, -1.0f32],
--           [0.0f32, 0.0f32, 0.0f32, 0.0f32]]
--        }

def obj (x: [8]f32) =
  #[unsafe]
  -- For simplicity of generated code.
  let col_w_pre_red =
    tabulate_3d 4 2 4 (\k i j -> x[k + j] * x[i + j])
  let col_w_red =
    map (map f32.sum) col_w_pre_red
  let col_eq: [4]f32 =
    map (\w -> w[0] - w[1]) col_w_red
  in col_eq

entry fwd_J (x: [8]f32) =
  tabulate 8 (\i -> jvp obj x (replicate 8 0 with [i] = 1))

entry rev_J (x: [8]f32) =
  transpose (tabulate 4 (\i -> vjp obj x (replicate 4 0 with [i] = 1)))

entry fwd_vec_J (x: [8]f32) =
  let seeds = tabulate 8 (\i -> replicate 8 0 with [i] = 1)
  in jmp obj x seeds

entry rev_vec_J (x: [8]f32) =
  let seeds = tabulate 4 (\i -> replicate 4 0 with [i] = 1)
  in transpose (mjp obj x seeds)
