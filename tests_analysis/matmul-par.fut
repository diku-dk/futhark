-- Matrix multiplication
def main [n] (xss: [n][n]i32) (yss: [n][n]i32) : [n][n]i32 =
    map (\xs_row ->
        map (\ys_col ->
            -- #[sequential]
            reduce (+) 0 (map2 (*) xs_row ys_col)
        ) (transpose yss)
    ) xss

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5596 : {
--     (arr) xss_5385 : {
--         (idx) eta_p_5606 :
--             0 : dependencies = {gtid_5597 0 par}
--             1 : dependencies = {i_5724 2 seq}
--     }
--     (arr) yss_5386 : {
--         (idx) eta_p_5607 :
--             0 : dependencies = {i_5724 2 seq}
--             1 : dependencies = {i_5721 1 seq}
--     }
-- }
-- (segmap) defunc_0_map_res_5685 : {
--     (arr) xss_5385 : {
--         (idx) eta_p_5695 :
--             0 : dependencies = {gtid_5686 0 par}
--             1 : dependencies = {i_5726 1 seq}
--     }
--     (arr) yss_5386 : {
--         (idx) eta_p_5696 :
--             0 : dependencies = {i_5726 1 seq}
--             1 : dependencies = {gtid_5687 0 par}
--     }
-- }
-- (segred) defunc_0_map_res_5704 : {
--     (arr) xss_5385 : {
--         (idx) eta_p_5714 :
--             0 : dependencies = {gtid_5705 0 par}
--             1 : dependencies = {gtid_5707 0 par}
--     }
--     (arr) yss_5386 : {
--         (idx) eta_p_5715 :
--             0 : dependencies = {gtid_5707 0 par}
--             1 : dependencies = {gtid_5706 0 par}
--     }
-- }
