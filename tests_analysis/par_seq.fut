def main [n][m] (xss: [n][m]i64) : [n]i64 =
  map (\xs ->
    #[unsafe]
    loop s=0 for i < m
    do
      s + xs[i]
  ) xss


-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5185 : {
--     (arr) xss_5128 : {
--         (idx) +_rhs_5192 :
--             0 : dependencies = {gtid_5186 0 par}
--             1 : dependencies = {i_5190 1 seq}
--     }
-- }
