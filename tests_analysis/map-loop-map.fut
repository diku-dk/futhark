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
--             0 : dependencies = [ gtid_5771 1 par ]
--             1 : dependencies = [ i_5929 3 seq ]
--             2 : dependencies = [ i_5775 2 seq ]
--     }
-- }
-- (segmap) defunc_0_f_res_5797 : {
--     (arr) xsss_5497 : {
--         (idx) xsss_transformed_row_5803 :
--             0 : dependencies = [ gtid_5790 1 par ]
--             1 : dependencies = [ gtid_5798 3 par ]
--             2 : dependencies = [ i_5794 2 seq ]
--     }
-- }
-- (segmap) lifted_lambda_res_5886 : {
--     (arr) xsss_5497 : {
--         (idx) xsss_transformed_row_5896 :
--             0 : dependencies = [ gtid_5887 2 par ]
--             1 : dependencies = [ i_5931 3 seq ]
--             2 : dependencies = [ i_5878 1 seq ]
--     }
--     (arr) s_expanded_5879 : {
--         (idx) s_5890 :
--             0 : dependencies = [ gtid_5887 2 par ]
--     }
-- }
-- (segmap) lifted_lambda_res_5901 : {
--     (arr) xsss_5497 : {
--         (idx) xsss_transformed_row_5911 :
--             0 : dependencies = [ gtid_5902 2 par ]
--             1 : dependencies = [ i_5933 3 seq ]
--             2 : dependencies = [ i_5878 1 seq ]
--     }
--     (arr) s_expanded_5879 : {
--         (idx) s_5905 :
--             0 : dependencies = [ gtid_5902 2 par ]
--     }
-- }
