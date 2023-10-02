def main [m][n] (xs: [m][n]i64) (is: [m]i64) (is1: [n]i64) : [m][n]i64 =
  map (\i ->
    map (\j ->
      #[unsafe] xs[is[i+2],is1[j*3]]
      ) (iota n)
    ) (iota m)

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5635 : {
--     (arr) xs_5425 : {
--         (idx) lifted_lambda_res_5643 :
--             0 : dependencies = {gtid_5636 0 par}
--             1 : dependencies = {gtid_5637 1 par}
--     }
-- }
