def main [l][n][m] (xsss: [l][n][m]i64) : [l][n]i64 =
  map (\xss ->
    map (reduce (+) 0) xss
  ) xsss

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5361 : {
--     (arr) xsss_5239 : {
--         (idx) x_5371 :
--             0 : dependencies = [ gtid_5362 1 par ]
--             1 : dependencies = [ i_5472 2 seq ]
--             2 : dependencies = [ i_5475 3 seq ]
--     }
-- }
-- (segmap) defunc_0_map_res_5441 : {
--     (arr) xsss_5239 : {
--         (idx) x_5450 :
--             0 : dependencies = [ gtid_5442 2 par ]
--             1 : dependencies = [ gtid_5443 2 par ]
--             2 : dependencies = [ i_5477 3 seq ]
--     }
-- }
-- (segred) defunc_0_map_res_5457 : {
--     (arr) xsss_5239 : {
--         (idx) x_5467 :
--             0 : dependencies = [ gtid_5458 2 par ]
--             1 : dependencies = [ gtid_5459 2 par ]
--             2 : dependencies = [ gtid_5460 2 par ]
--     }
-- }
