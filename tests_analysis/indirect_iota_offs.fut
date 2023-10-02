def main [m][n] (xs: [m][n]i64) (is: [m]i64) (is1: [n]i64) : [m][n]i64 =
  map (\i ->
    map (\j ->
      #[unsafe] xs[is[i+2],is1[j*3]]
      ) (iota n)
    ) (iota m)

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
