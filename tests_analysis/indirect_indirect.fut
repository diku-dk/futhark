def main [n][m] (xs: [n]i64) (is_0: [m]i64) (is_1: [m]i64) : [n]i64 =
  map (\i -> #[unsafe] xs[is_0[is_1[i]]]) (iota n)

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5324 : {
--     (arr) xs_5241 : {
--         (idx) lifted_lambda_res_5329 :
--             0 : dependencies = [ gtid_5325 0 par ]
--     }
--     (arr) is_0_5242 : {
--         (idx) tmp_5328 :
--             0 : dependencies = [ gtid_5325 0 par ]
--     }
--     (arr) is_1_5243 : {
--         (idx) is_1_elem_5327 :
--             0 : dependencies = [ gtid_5325 0 par ]
--     }
-- }
