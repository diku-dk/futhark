def main [l][n][m] (xsss: [l][n][m]i64) : [l]i64 =
    map2 (\xss i -> #[unsafe] xss[0,i]) xsss (iota l)

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5384 : {
--     (arr) xsss_5302 : {
--         (idx) lifted_lambda_res_5389 :
--             0 : dependencies = [ gtid_5385 0 par ]
--             1 : dependencies = [  ]
--             2 : dependencies = [ gtid_5385 0 par ]
--     }
-- }
