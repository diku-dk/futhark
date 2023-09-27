def main [l][m][n] (xsss: [l][m][n]i64) : [l][m]i64 =
    map (\i ->
        map (\j ->
            #[unsafe]
            reduce (+) 0 xsss[i+j][i*j]
        ) (iota m)
    ) (iota l)

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5586 : {
--     (arr) xsss_5393 : {
--         (idx) x_5599 :
--             0 : dependencies = {gtid_5587 0 par, i_5835 1 seq}
--             1 : dependencies = {gtid_5587 0 par, i_5835 1 seq}
--             2 : dependencies = {i_5838 2 seq}
--     }
-- }
-- (segmap) defunc_0_map_res_5742 : {
--     (arr) xsss_5393 : {
--         (idx) x_5755 :
--             0 : dependencies = {gtid_5743 0 par, gtid_5744 1 par}
--             1 : dependencies = {gtid_5743 0 par, gtid_5744 1 par}
--             2 : dependencies = {i_5840 2 seq}
--     }
-- }
-- (segred) defunc_0_map_res_5790 : {
--     (arr) xsss_5393 : {
--         (idx) x_5800 :
--             0 : dependencies = {gtid_5791 0 par, gtid_5792 1 par}
--             1 : dependencies = {gtid_5791 0 par, gtid_5792 1 par}
--             2 : dependencies = {gtid_5793 2 par}
--     }
-- }
