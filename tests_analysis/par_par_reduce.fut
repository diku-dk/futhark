def main [l][n][m] (xsss: [l][n][m]i64) : [l][n]i64 =
  map (\xss ->
    map (reduce (+) 0) xss
  ) xsss

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5361 : {
--     (arr) xsss_5239 : {
--         (idx) x_5371 :
--             0 : dependencies = {gtid_5362 0 par}
--             1 : dependencies = {i_5472 1 seq}
--             2 : dependencies = {i_5475 2 seq}
--     }
-- }
-- (segmap) defunc_0_map_res_5441 : {
--     (arr) xsss_5239 : {
--         (idx) x_5450 :
--             0 : dependencies = {gtid_5442 0 par}
--             1 : dependencies = {gtid_5443 0 par}
--             2 : dependencies = {i_5477 1 seq}
--     }
-- }
-- (segred) defunc_0_map_res_5457 : {
--     (arr) xsss_5239 : {
--         (idx) x_5467 :
--             0 : dependencies = {gtid_5458 0 par}
--             1 : dependencies = {gtid_5459 0 par}
--             2 : dependencies = {gtid_5460 0 par}
--     }
-- }
