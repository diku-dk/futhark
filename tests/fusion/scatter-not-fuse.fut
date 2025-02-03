-- ==
-- compiled input {
--   [3i64, -1i64, 1i64, 5i64, 2i64, -1i64, 7i64, 6i64]
--   [7.0f32, 8.0f32, 9.0f32, 10.0f32, 12.0f32, 15.0f32, 18.0f32, 11.0f32]
-- }
-- output {
--   [50.0f32, 0.0f32, 40.0f32, 75.0f32, 45.0f32, 0.0f32, 55.0f32, 90.0f32]
--   [21.0f32, 0.0f32, 0.0f32, 0.0f32, 36.0f32, 0.0f32, 0.0f32, 0.0f32]
-- }

def main [n] (is : [n]i64) (ys_bar: *[n]f32) =
  let scatter_res_adj_gather =
    map (\ is_elem -> if is_elem >= 0 && is_elem < n
               then ys_bar[is_elem] else 0
      ) is
  let zeros = replicate n 0.0f32
  let map_res_bar = scatter ys_bar is zeros
  let map_adjs_1 =
    map (\ lam_adj -> 5.0f32 * lam_adj ) scatter_res_adj_gather
  let map_adjs_2 =
    map (\ lam_adj -> 3.0f32 * lam_adj ) map_res_bar
  in (map_adjs_1, map_adjs_2)
