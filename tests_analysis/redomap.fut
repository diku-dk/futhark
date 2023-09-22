def main [n][m] (xss: [n][m]i64) : [n]i64 =
  map (reduce (+) 0) xss

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5209 : {
--     (arr) xss_5145 : {
--         (idx) x_5217 :
--             0 : dependencies = [ gtid_5210 1 par ]
--             1 : dependencies = [ i_5252 2 seq ]
--     }
-- }
-- (segred) defunc_0_map_res_5241 : {
--     (arr) xss_5145 : {
--         (idx) x_5249 :
--             0 : dependencies = [ gtid_5242 1 par ]
--             1 : dependencies = [ gtid_5243 1 par ]
--     }
-- }
