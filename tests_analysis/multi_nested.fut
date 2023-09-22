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
-- (segmap) defunc_0_map_res_5839 : {
--     (arr) xsss_5483 : {
--         (idx) foldl_arg2_5856 :
--             0 : dependencies = [ gtid_5840 0 par | gtid_5841 0 par | k_5848 1 seq ]
--             1 : dependencies = [ h_5853 2 seq ]
--             2 : dependencies = [ gtid_5840 0 par | gtid_5841 0 par | k_5848 1 seq | h_5853 2 seq ]
--     }
-- }
