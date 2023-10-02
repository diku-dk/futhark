def main [m][n] (xs: [m][n]i64) (is: [m]i64) (is1: [n]i64) : [m][n]i64 =
  map (\i ->
    map (\j ->
      #[unsafe]
      let k = 5
      in xs[is[i*k],is1[j+k]]
      ) (iota n)
    ) (iota m)

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5636 : {
--     (arr) xs_5426 : {
--         (idx) lifted_lambda_res_5644 :
--             0 : dependencies = {gtid_5637 0 par}
--             1 : dependencies = {gtid_5638 1 par}
--     }
-- }
