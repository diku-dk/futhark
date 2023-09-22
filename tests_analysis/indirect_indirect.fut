def main [n][m] (xs: [n]i64) (is_0: [m]i64) (is_1: [m]i64) : [n]i64 =
  map (\i -> #[unsafe] xs[is_0[is_1[i]]]) (iota n)

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5324 : {
--     (arr) is_1 : {
--         (idx) is_1_elem :
--             0 : dependencies = [ gtid_5325 0 par ]
--     }
--     (arr) is_0 : {
--         (idx) tmp :
--             0 : dependencies = [ gtid_5325 0 par ]
--     }
--     (arr) xs_5241 : {
--         (idx) lifted_lambda_res :
--             0 : dependencies = [ gtid_5325 0 par ]
--     }
-- }
