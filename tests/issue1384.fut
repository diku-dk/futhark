def loess_proc [n_m] (q: i64) (m_fun: i64 -> i64) (max_dist: [n_m]f32) : ([n_m]f32, [n_m]f32) =
  (max_dist, max_dist)

def loess_l [m] [n] [n_m]
            (xx_l: [m][n]f32)
            (yy_l: [m][n]f32)
            (q: i64)
            (m_fun: i64 -> i64)
            (ww_l: [m][n]f32)
            (l_idx_l: [m][n_m]i64)
            (max_dist_l: [m][n_m]f32)
            (n_nn_l: [m]i64) : ([m][n_m]f32, [m][n_m]f32) =
  let loess_l_fun_fun (q_pad: i64) =
    map3 (\xx yy max_dist -> loess_proc q m_fun max_dist)
         xx_l
         yy_l
         max_dist_l
    |> unzip
  in if q < 12
     then loess_l_fun_fun 11
     else if q < 32
     then loess_l_fun_fun 31
     else if q < 64
     then loess_l_fun_fun 63
     else loess_l_fun_fun 4095

entry main [m] [n] [n_m]
           (xx_l: [m][n]f32)
           (yy_l: [m][n]f32)
           (ww_l: [m][n]f32)
           (l_idx_l: [m][n_m]i64)
           (max_dist_l: [m][n_m]f32)
           (n_nn_l: [m]i64)
           (q: i64)
           (jump: i64) : ([m][n_m]f32, [m][n_m]f32) =
  let m_fun (x: i64): i64 = 2 + i64.min (x * jump) (n - 1)
  in loess_l xx_l
             yy_l
             q
             m_fun
             ww_l
             l_idx_l
             max_dist_l
             n_nn_l
