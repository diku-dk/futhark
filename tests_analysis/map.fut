entry main [n] (xs: [n]i64) : [n]i64 =
  map ((+) 2) xs

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5119 : {
--     (arr) xs_5088 : {
--         (idx) eta_p_5122 :
--             0 : dependencies = [ gtid_5120 0 par ]
--     }
-- }
