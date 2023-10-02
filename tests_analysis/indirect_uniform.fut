def main [m][n] (xs: [m][n]i64) (is: [m]i64) (is1: [n]i64) : [m][n]i64 =
  map (\i ->
    map (\j ->
      #[unsafe] xs[is[i],is1[j]]
      ) (iota n)
    ) (iota m)

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5253 : {
--     (arr) xs_5191 : {
--         (idx) lifted_lambda_res_5257 :
--             0 : dependencies = {gtid_5254 0 par}
--     }
--     (arr) is_5192 : {
--         (idx) is_elem_5256 :
--             0 : dependencies = {gtid_5254 0 par}
--     }
-- }
