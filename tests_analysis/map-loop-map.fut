entry main [m][n][o] (xsss: [m][n][o]i64) : [m]i64 =
  map ( \x ->
      #[unsafe]
      loop s = 0 for i < o do
        reduce (+) s <| map ( \y ->
           i + xsss[x][y][i]
        ) (iota n)

  ) <| iota m

-- === Expected output of analysis:
-- (segmap) defunc_0_f_res_5774 : {
--     (arr) xsss_5497 : {
--         (idx) xsss_transformed_row_5786 :
--             0 : dependencies = {gtid_5775 0 par}
--             1 : dependencies = {i_5933 2 seq}
--             2 : dependencies = {i_5779 1 seq}
--     }
-- }
-- (segmap) defunc_0_f_res_5793 : {
--     (arr) xsss_5497 : {
--         (idx) xsss_transformed_row_5807 :
--             0 : dependencies = {gtid_5794 0 par}
--             1 : dependencies = {gtid_5802 2 par}
--             2 : dependencies = {i_5798 1 seq}
--     }
-- }
-- (segmap) lifted_lambda_res_5890 : {
--     (arr) xsss_5497 : {
--         (idx) xsss_transformed_row_5900 :
--             0 : dependencies = {gtid_5891 1 par}
--             1 : dependencies = {i_5935 2 seq}
--             2 : dependencies = {i_5882 0 seq}
--     }
-- }
-- (segmap) lifted_lambda_res_5905 : {
--     (arr) xsss_5497 : {
--         (idx) xsss_transformed_row_5915 :
--             0 : dependencies = {gtid_5906 1 par}
--             1 : dependencies = {i_5937 2 seq}
--             2 : dependencies = {i_5882 0 seq}
--     }
-- }
-- (segred) defunc_0_f_res_5801 : {
--     (arr) xsss_5497 : {
--         (idx) xsss_transformed_row_5807 :
--             0 : dependencies = {gtid_5794 0 par}
--             1 : dependencies = {gtid_5802 2 par}
--             2 : dependencies = {i_5798 1 seq}
--     }
-- }
