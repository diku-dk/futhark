def main [n][m] (xss: [n][m]i64) (is: [n]i64) : [n][m]i64 =
  let k = foldl (+) 0 (map (\i -> i*0) is)
  let l = map (\i -> i*k) is
  in
  map (\_ ->
    #[unsafe]
    loop s=xss[0] for i < n
    do
      map2 (+) s xss[ l[i]+i ]
  ) xss


-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5658 : {
--     (arr) xss_5482 : {
--         (idx) eta_p_5662 :
--             0 : dependencies = [ i_5629 0 seq ]
--             1 : dependencies = [ gtid_5659 1 par ]
--     }
--     (arr) s_5630 : {
--         (idx) eta_p_5661 :
--             0 : dependencies = [ gtid_5659 1 par ]
--     }
-- }
