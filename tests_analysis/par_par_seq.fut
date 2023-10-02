def main [l][n][m] (xsss: [l][n][m]i64) : [l][n]i64 =
  map (\xss ->
    map (foldl (+) 0) xss
  ) xsss

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5381 : {
--     (arr) xsss_5238 : {
--         (idx) b_5389 :
--             0 : dependencies = {gtid_5382 0 par}
--             1 : dependencies = {gtid_5383 1 par}
--             2 : dependencies = {i_5387 2 seq}
--     }
-- }
