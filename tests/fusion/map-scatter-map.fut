-- ==
-- compiled input {
--   [3i64, -1i64, 1i64, 5i64, 2i64, -1i64, 7i64, 6i64]
--   [7.0f32, 8.0f32, 9.0f32, 10.0f32, 12.0f32, 15.0f32, 18.0f32, 11.0f32]
--   [1.0f32, 3.0f32, 2.0f32, 5.0f32, 4.0f32, 7.0f32, 9.0f32, 8.0f32]
--   [5.0f32, 9.0f32, 22.0f32, 33.0f32, 27.0f32, 22.0f32, 17.0f32, 8.0f32]
-- }
-- output {
--   [231735.0f32, 69984.0f32, 518400.0f32,  3626700.0f32, 528768.0f32, 2310000.0f32, 7840800.0f32, 1.28304e7f32]
--   [811930.0f32, 93312.0f32, 1166400.0f32, 3626700.0f32, 886464.0f32, 2475000.0f32, 7840800.0f32, 8820900.0f32]
-- }

def main [n] (is: [n]i64) (vs: [n]f32) (xs: [n]f32) (ys_bar: *[n]f32) =
  let map_res_1 = map2 (*) xs vs
  let zip_copy = copy map_res_1
  let map_res_2 = map2 (*) vs zip_copy
  let scatter_res_1 = scatter map_res_1 is map_res_2
  let (map_adjs_1, map_adjs_2) =
    unzip
    <| map3 (\x y lam_adj -> (y * lam_adj, x * lam_adj))
            map_res_2
            scatter_res_1
            ys_bar
  let scatter_res_adj_gather =
    map (\is_elem ->
           if is_elem >= 0 && is_elem < n
           then map_adjs_2[is_elem]
           else 0.0f32)
        is
  let map_res_adj_1 =
    map2 (+) map_adjs_1 scatter_res_adj_gather
  let map_res_bar =
    scatter map_adjs_2 is (replicate n 0.0f32)
  let (map_adjs_3, map_adjs_4) =
    unzip
    <| map3 (\x y lam_adj -> (y * lam_adj, x * lam_adj))
            vs
            zip_copy
            map_res_adj_1
  let map_res_adj_2 = map2 (+) map_res_bar map_adjs_4
  let (map_adjs_5, map_adjs_6) =
    unzip
    <| map3 (\x y lam_adj -> (y * lam_adj, x * lam_adj))
            xs
            vs
            map_res_adj_2
  let x_adj = map2 (+) map_adjs_3 map_adjs_6
  in (x_adj, map_adjs_5)
