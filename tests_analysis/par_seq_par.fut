def main [l][n][m] (xsss: [l][n][m]i64) : [l][m]i64 =
  map (\xss ->
    #[unsafe]
    loop _=xss[0] for i < n do
      map (\x -> x*2) xss[i]
  ) xsss

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5333 : {
--     (arr) xsss_5223 : {
--         (idx) eta_p_5347 :
--             0 : dependencies = [ gtid_5334 1 par ]
--             1 : dependencies = [ i_5341 2 seq ]
--             2 : dependencies = [ gtid_5345 3 par ]
--         (idx) xsss_transformed_row_5338 :
--             0 : dependencies = [ gtid_5334 1 par ]
--             1 : dependencies = [  ]
--     }
-- }
-- (segmap) lifted_lambda_res_5412 : {
--     (arr) xsss_5223 : {
--         (idx) eta_p_5416 :
--             0 : dependencies = [ gtid_5413 2 par ]
--             1 : dependencies = [ i_5401 1 seq ]
--             2 : dependencies = [ gtid_5414 2 par ]
--     }
-- }
-- (segmap) defunc_0_map_res_5344 : {
--     (arr) xsss_5223 : {
--         (idx) eta_p_5347 :
--             0 : dependencies = [ gtid_5334 1 par ]
--             1 : dependencies = [ i_5341 2 seq ]
--             2 : dependencies = [ gtid_5345 3 par ]
--     }
-- }
