def main [n][m] (xss: [n][m]i64) : [m]i64 =
    #[unsafe]
    loop res=replicate m 0 for i < n
    do
        map2 (\x r -> x+r) xss[i] res

-- === Expected output of analysis: CONFIRMED
-- (segmap) defunc_0_map_res_5348 : {
--     (arr) xss_5280 : {
--         (idx) eta_p_5351 :
--             0 : dependencies = [ i_5325 0 seq ]
--             1 : dependencies = [ gtid_5349 1 par ]
--     }
--     (arr) res_5326 : {
--         (idx) eta_p_5352 :
--             0 : dependencies = [ gtid_5349 1 par ]
--     }
-- }
