def main [n][m] (xss: [n][m]i64) : [m]i64 =
  #[unsafe]
  map (\x -> x*2) xss[0]

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5148 : {
--     (arr) xss_5111 : {
--         (idx) eta_p_5151 :
--             0 : dependencies = {}
--             1 : dependencies = {gtid_5149 0 par}
--     }
-- }