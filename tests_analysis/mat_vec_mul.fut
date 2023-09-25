-- Matrix-vector multiplication
def main [n] (xss: [n][n]i32) (v: [n]i32) : [n]i32 =
    let vs = replicate n v
    in map2 (\A_row vs_col ->
        foldl (+) 0 (map2 (*) A_row vs_col)
    ) xss vs

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_r_5726 : {
--     (arr) xss_5513 : {
--         (idx) eta_p_5730 :
--             0 : dependencies = [ gtid_5727 0 par ]
--             1 : dependencies = [ gtid_5728 0 par ]
--     }
--     (arr) v_5514 : {
--         (idx) eta_p_5731 :
--             0 : dependencies = [ gtid_5728 0 par ]
--     }
-- }
-- (segmap) defunc_0_map_res_5737 : {
--     (arr) xss_5513 : {
--         (idx) eta_p_5730 :
--             0 : dependencies = [ gtid_5727 0 par ]
--             1 : dependencies = [ gtid_5728 0 par ]
--     }
--     (arr) v_5514 : {
--         (idx) eta_p_5731 :
--             0 : dependencies = [ gtid_5728 0 par ]
--     }
--     (arr) defunc_0_map_res_r_5726 : {
--         (idx) defunc_0_map_res_5740 :
--             0 : dependencies = [ gtid_5738 0 par ]
--             1 : dependencies = [  ]
--     }
-- }
