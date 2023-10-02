def main [l][m][n][o] (xsss: [l][m][n][o]i64) : [l][m]i64 =
    map (\i ->
        map (\j ->
            #[unsafe]
            loop r=0 for k < n do
                r + loop r=0 for h < n do
                    r + foldl (+) 0 xsss[i+j+k][h][i*j*k*h]
        ) (iota m)
    ) (iota l)

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5832 : {
--     (arr) xsss_5483 : {
--         (idx) b_5852 :
--             0 : dependencies = {gtid_5833 0 par, gtid_5834 1 par, k_5841 2 seq}
--             1 : dependencies = {h_5846 3 seq}
--             2 : dependencies = {gtid_5833 0 par, gtid_5834 1 par, k_5841 2 seq, h_5846 3 seq}
--             3 : dependencies = {i_5850 4 seq}
--     }
-- }
