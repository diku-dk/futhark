-- #1878.  The interesting thing here is that the sparse adjoint also
-- has active free variables.
-- ==
-- tags { autodiff }
-- entry: fwd_map fwd_vec rev_map rev_vec
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

entry fwd_map (x: [8]f32) =
  tabulate 8 (\i -> jvp obj x (replicate 8 0 with [i] = 1))

entry fwd_vec (x: [8]f32) =
  jmp obj x (tabulate 8 (\i -> replicate 8 0 with [i] = 1))

entry rev_map (x: [8]f32) =
  transpose (tabulate 4 (\i -> vjp obj x (replicate 4 0 with [i] = 1)))

entry rev_vec (x: [8]f32) =
  transpose (#[unroll] mjp obj x (tabulate 4 (\i -> replicate 4 0 with [i] = 1)))
