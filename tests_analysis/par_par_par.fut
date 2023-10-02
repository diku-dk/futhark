def main [l][n][m] (xsss: [l][n][m]i64) : [l][n][m]i64 =
  map (\xss ->
    map (\xs -> map (+2) xs) xss
  ) xsss

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5485 : {
--     (arr) xsss_5281 : {
--         (idx) eta_p_5490 :
--             0 : dependencies = {gtid_5486 0 par}
--             1 : dependencies = {gtid_5487 1 par}
--             2 : dependencies = {gtid_5488 2 par}
--     }
-- }
