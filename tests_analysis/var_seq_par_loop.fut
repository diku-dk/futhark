def main [n][m] (xss: [n][m]i64) : [m]i64 =
    #[unsafe]
    loop res=replicate m 0 for i < n
    do
        map2 (\x r -> x+r) xss[i] res

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5350 : {
--     (arr) xss_5280 : {
--         (idx) eta_p_5353 :
--             0 : dependencies = {i_5325 0 seq}
--             1 : dependencies = {gtid_5351 1 par}
--     }
-- }
