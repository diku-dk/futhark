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
--             0 : dependencies = {gtid_5676 0 par}
--             1 : dependencies = {gtid_5678 2 par}
--     }
--     (arr) yss_5385 : {
--         (idx) eta_p_5681 :
--             0 : dependencies = {gtid_5678 2 par}
--             1 : dependencies = {gtid_5677 1 par}
--     }
-- }
-- (segmap) defunc_0_map_res_5688 : {
--     (arr) xss_5384 : {
--         (idx) eta_p_5680 :
--             0 : dependencies = {gtid_5676 0 par}
--             1 : dependencies = {gtid_5678 2 par}
--     }
--     (arr) yss_5385 : {
--         (idx) eta_p_5681 :
--             0 : dependencies = {gtid_5678 2 par}
--             1 : dependencies = {gtid_5677 1 par}
--     }
--     (arr) defunc_0_map_res_r_r_5675 : {
--         (idx) b_5696 :
--             0 : dependencies = {gtid_5689 2 par}
--             1 : dependencies = {gtid_5690 3 par}
--             2 : dependencies = {i_5694 4 seq}
--     }
-- }