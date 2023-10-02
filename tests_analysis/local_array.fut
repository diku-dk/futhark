def main [m][n] (xss: [m][n]i32) =
  map (\xs ->
    map (\x ->
      let xx = #[sequential] scan (+) 0 xs
      in #[unsafe] xx with [xx[x]] = 4243
    ) xs
  ) xss

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5510 : {
--     (arr) xss_5294 : {
--         (idx) eta_p_5515 :
--             0 : dependencies = {gtid_5511 0 par}
--             1 : dependencies = {gtid_5512 1 par}
--         (idx) x_5520 :
--             0 : dependencies = {gtid_5511 0 par}
--             1 : dependencies = {i_5529 2 seq}
--     }
-- }
