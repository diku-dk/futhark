-- def main [n][m] (xs: [n]i64) (is_0: [m]i64) (is_1: [m]i64) : [n]i64 =
--   map (\i -> #[unsafe] xs[is_0[is_1[i]]]) (iota n)
  def main [m][n] (xs: [m][n]i64) (is: [m]i64) (is1: [n]i64) : [m][n]i64 =
  map (\i ->
    map (\j ->
      #[unsafe] xs[is[is1[i]],is1[is[j]]]
      ) (iota n)
    ) (iota m)

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5647 : {
--     (arr) xs_5437 : {
--         (idx) lifted_lambda_res_5654 :
--             0 : dependencies = {gtid_5648 0 par}
--             1 : dependencies = {gtid_5649 1 par}
--     }
-- }
