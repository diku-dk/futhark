-- #1878
-- ==
-- tags { autodiff }
-- entry: fwd_map fwd_vec rev_map rev_vec
-- input { [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0] }
-- output { [[0.0, 2.0, 3.0, 4.0],
--           [0.0, 0.0, 1.0, 1.0],
--           [0.0, 0.0, 0.0, 1.0],
--           [0.0, 0.0, 0.0, 0.0],
--           [-4.0, -6.0, -7.0, -8.0],
--           [0.0, 0.0, -1.0, -1.0],
--           [0.0, 0.0, 0.0, -1.0],
--           [0.0, 0.0, 0.0, 0.0]]
--        }

def obj (x: [8]f64) =
  #[unsafe]
  -- For simplicity of generated code.
  let col_w_pre_red =
    tabulate_3d 4 2 4 (\k i j -> x[k + j] * x[i + j])
  let col_w_red =
    map (map f64.sum) col_w_pre_red
  let col_eq: [4]f64 =
    map (\w -> w[0] - w[1]) col_w_red
  in col_eq

entry fwd_map (x: [8]f64) =
  tabulate 8 (\i -> jvp obj x (replicate 8 0 with [i] = 1))

entry fwd_vec (x: [8]f64) =
  #[unroll]
  jvp_vec obj x (tabulate 8 (\i -> replicate 8 0 with [i] = 1))

entry rev_map (x: [8]f64) =
  transpose (tabulate 4 (\i -> vjp obj x (replicate 4 0 with [i] = 1)))

entry rev_vec (x: [8]f64) =
  transpose (#[unroll] vjp_vec obj x (tabulate 4 (\i -> replicate 4 0 with [i] = 1)))
