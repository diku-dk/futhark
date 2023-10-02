entry main [m][n][o] (xsss: [m][n][o]i64) : [m]i64 =
  map ( \x ->
      #[unsafe]
      loop s = 0 for i < o do
        reduce (+) s <| map ( \y ->
           i + xsss[x][y][i]
        ) (iota n)

  ) <| iota m

-- === Expected output of analysis:
-- (segmap) defunc_0_f_res_5770 : {
--     (arr) xsss_5497 : {
--         (idx) xsss_transformed_row_5782 :
--             0 : dependencies = {gtid_5771 0 par}
--             1 : dependencies = {i_5929 2 seq}
--             2 : dependencies = {i_5775 1 seq}
--     }
-- }
-- (segmap) defunc_0_f_res_5789 : {
--     (arr) xsss_5497 : {
--         (idx) xsss_transformed_row_5803 :
--             0 : dependencies = {gtid_5790 0 par}
--             1 : dependencies = {gtid_5798 2 par}
--             2 : dependencies = {i_5794 1 seq}
--     }
-- }
-- (segmap) lifted_lambda_res_5886 : {
--     (arr) xsss_5497 : {
--         (idx) xsss_transformed_row_5896 :
--             0 : dependencies = {gtid_5887 1 par}
--             1 : dependencies = {i_5931 2 seq}
--             2 : dependencies = {i_5878 0 seq}
--     }
-- }
-- (segmap) lifted_lambda_res_5901 : {
--     (arr) xsss_5497 : {
--         (idx) xsss_transformed_row_5911 :
--             0 : dependencies = {gtid_5902 1 par}
--             1 : dependencies = {i_5933 2 seq}
--             2 : dependencies = {i_5878 0 seq}
--     }
-- }
-- (segred) defunc_0_f_res_5797 : {
--     (arr) xsss_5497 : {
--         (idx) xsss_transformed_row_5803 :
--             0 : dependencies = {gtid_5790 0 par}
--             1 : dependencies = {gtid_5798 2 par}
--             2 : dependencies = {i_5794 1 seq}
--     }
-- }
