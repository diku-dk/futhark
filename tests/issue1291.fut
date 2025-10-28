def main [n] (is: [n]i64) (ys_bar: *[n]f32) =
  let scatter_res_adj_gather =
    map (\is_elem -> ys_bar[is_elem]) is
  let zeros = replicate n 0.0f32
  let map_res_bar = scatter ys_bar is zeros
  let map_adjs_1 =
    map (\lam_adj -> 5.0f32 * lam_adj) scatter_res_adj_gather
  let map_adjs_2 =
    map (\lam_adj -> 3.0f32 * lam_adj) map_res_bar
  in (map_adjs_1, map_adjs_2)
