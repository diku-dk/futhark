def main [n][m] (xss: [n][m]i64) : [n]i64 =
  map (foldl (+) 0) xss

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5204 : {
--     (arr) xss_5144 : {
--         (idx) b_5211 :
--             0 : dependencies = {gtid_5205 0 par}
--             1 : dependencies = {i_5209 1 seq}
--     }
-- }
