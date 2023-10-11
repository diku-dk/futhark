def main [l][m][n] (xsss: [l][m][n]i64) : [l][m]i64 =
    map (\i ->
        map (\j ->
            #[unsafe]
            reduce (+) 0 xsss[i+j][i*j]
        ) (iota m)
    ) (iota l)

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5588 : {
--     (arr) xsss_5393 : {
--         (idx) x_5601 :
--             0 : dependencies = {gtid_5589 0 par, i_5837 1 seq}
--             1 : dependencies = {gtid_5589 0 par, i_5837 1 seq}
--             2 : dependencies = {i_5840 2 seq}
--     }
-- }
-- (segmap) defunc_0_map_res_5744 : {
--     (arr) xsss_5393 : {
--         (idx) x_5757 :
--             0 : dependencies = {gtid_5745 0 par, gtid_5746 1 par}
--             1 : dependencies = {gtid_5745 0 par, gtid_5746 1 par}
--             2 : dependencies = {i_5842 2 seq}
--     }
-- }
-- (segred) defunc_0_map_res_5792 : {
--     (arr) xsss_5393 : {
--         (idx) x_5802 :
--             0 : dependencies = {gtid_5793 0 par, gtid_5794 1 par}
--             1 : dependencies = {gtid_5793 0 par, gtid_5794 1 par}
--             2 : dependencies = {gtid_5795 2 par}
--     }
-- }
