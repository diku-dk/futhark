def main [l][n][m] (xsss: [l][n][m]i64) : [l][m]i64 =
  -- #[incremental_flattening(no_intra)]
  -- #[incremental_flattening(no_outer)]
  map (\xss ->
    #[unsafe]
    loop _=xss[0] for i < n do
      map (\x -> x*2) xss[i]
  ) xsss

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5337 : {
--     (arr) xsss_5223 : {
--         (idx) eta_p_5351 :
--             0 : dependencies = {gtid_5338 0 par}
--             1 : dependencies = {i_5345 1 seq}
--             2 : dependencies = {gtid_5349 2 par}
--     }
-- }
-- (segmap) defunc_0_map_res_5348 : {
--     (arr) xsss_5223 : {
--         (idx) eta_p_5351 :
--             0 : dependencies = {gtid_5338 0 par}
--             1 : dependencies = {i_5345 1 seq}
--             2 : dependencies = {gtid_5349 2 par}
--     }
-- }
-- (segmap) lifted_lambda_res_5416 : {
--     (arr) xsss_5223 : {
--         (idx) eta_p_5420 :
--             0 : dependencies = {gtid_5417 1 par}
--             1 : dependencies = {i_5405 0 seq}
--             2 : dependencies = {gtid_5418 2 par}
--     }
-- }
