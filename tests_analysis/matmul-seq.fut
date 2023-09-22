-- Matrix multiplication
def main [n] (xss: [n][n]i32) (yss: [n][n]i32) : [n][n]i32 =
    map (\xs_row ->
        map (\ys_col ->
            foldl (+) 0 (map2 (*) xs_row ys_col)
        ) (transpose yss)
    ) xss

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_r_r_5675 : {
--     (arr) xss_5384 : {
--         (idx) eta_p_5680 :
--             0 : dependencies = [ gtid_5676 0 par ]
--             1 : dependencies = [ gtid_5678 0 par ]
--     }
--     (arr) yss_5385 : {
--         (idx) eta_p_5681 :
--             0 : dependencies = [ gtid_5678 0 par ]
--             1 : dependencies = [ gtid_5677 0 par ]
--     }
-- }
-- (segmap) defunc_0_map_res_5688 : {
--     (arr) defunc_0_map_res_r_r_5675 : {
--         (idx) defunc_0_map_res_5692 :
--             0 : dependencies = [ gtid_5689 0 par ]
--             1 : dependencies = [ gtid_5690 0 par ]
--     }
--     (arr) xss_5384 : {
--         (idx) eta_p_5680 :
--             0 : dependencies = [ gtid_5676 0 par ]
--             1 : dependencies = [ gtid_5678 0 par ]
--     }
--     (arr) yss_5385 : {
--         (idx) eta_p_5681 :
--             0 : dependencies = [ gtid_5678 0 par ]
--             1 : dependencies = [ gtid_5677 0 par ]
--     }
-- }
