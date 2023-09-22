def main [l][m][n] (xsss: [l][m][n]i64) : [l][m]i64 =
    map (\i ->
        map (\j ->
            #[unsafe]
            foldl (+) 0 xsss[i+j][i*j]
        ) (iota m)
    ) (iota l)

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5611 : {
--     (arr) xsss_5392 : {
--         (idx) foldl_arg2_5619 :
--             0 : dependencies = [ gtid_5612 0 par | gtid_5613 0 par ]
--             1 : dependencies = [ gtid_5612 0 par | gtid_5613 0 par ]
--     }
-- }
