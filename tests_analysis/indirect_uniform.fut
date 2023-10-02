def main [m][n] (xs: [m][n]i64) (is: [m]i64) (is1: [n]i64) : [m][n]i64 =
  map (\i ->
    map (\j ->
      #[unsafe] xs[is[i],is1[j]]
      ) (iota n)
    ) (iota m)

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5588 : {
--     (arr) xs_5413 : {
--         (idx) lifted_lambda_res_5594 :
--             0 : dependencies = {gtid_5589 0 par}
--             1 : dependencies = {gtid_5590 1 par}
--     }
-- }
