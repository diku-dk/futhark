def main [n] (xs: [n]i64) (is: [n]i64): [n]i64 =
  map (\i -> #[unsafe] xs[is[i]] ) (iota n)

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5253 : {
--     (arr) xs_5191 : {
--         (idx) lifted_lambda_res_5257 :
--             0 : dependencies = [ gtid_5254 0 par ]
--     }
--     (arr) is_5192 : {
--         (idx) is_elem_5256 :
--             0 : dependencies = [ gtid_5254 0 par ]
--     }
-- }
