  def main [m][n] (xs: [m][n]i64) (is: [m]i64) (is1: [n]i64) : [m][n]i64 =
  map (\i ->
    map (\j ->
      #[unsafe] xs[is[is1[i]],is1[is[j]]]
      ) (iota n)
    ) (iota m)

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5649 : {
--     (arr) xs_5437 : {
--         (idx) lifted_lambda_res_5656 :
--             0 : dependencies = {gtid_5650 0 par}
--             1 : dependencies = {gtid_5651 1 par}
--     }
-- }
