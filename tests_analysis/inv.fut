def main [n][m] (xss: [n][m]i64) : [n]i64 =
  map (\xs ->
    #[unsafe]
    loop s=0 for i < m
    do
      s + xs[ i*0 ]
  ) xss


-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5194 : {
--     (arr) xss_5134 : {
--         (idx) x_5199 :
--             0 : dependencies = [ gtid_5195 0 par ]
--             1 : dependencies = [  ]
--     }
-- }
