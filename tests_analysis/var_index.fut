def main (n: i64) m (A: [][]i64) =
  tabulate_2d  n m (\i j -> #[unsafe] A[j+1,i-1])


-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5681 : {
--     (arr) A_5449 : {
--         (idx) lifted_lambda_res_5688 :
--             0 : dependencies = {gtid_5683 1 par}
--             1 : dependencies = {gtid_5682 0 par}
--     }
-- }
