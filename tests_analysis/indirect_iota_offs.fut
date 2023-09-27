def main [n] (xs: [n]i64) (is: [n]i64) : [n]i64 =
  map (\i -> #[unsafe] xs[is[i+2]] ) (iota n)

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5262 : {
--     (arr) xs_5197 : {
--         (idx) lifted_lambda_res_5268 :
--             0 : dependencies = {gtid_5263 0 par}
--     }
--     (arr) is_5198 : {
--         (idx) tmp_5267 :
--             0 : dependencies = {gtid_5263 0 par}
--     }
-- }
