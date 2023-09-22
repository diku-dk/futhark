def main [n][m] (is: [m]i64) (xss: [n][m]i64) : [n]i64 =
  map (\xs ->
    #[unsafe]
    loop s=0 for i < m
    do
      s + xs[ is[ i ] ]
  ) xss


-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5231 : {
--     (arr) is_5157 : {
--         (idx) +_rhs_5238 :
--             0 : dependencies = [ i_5236 1 seq ]
--     }
--     (arr) xss_5158 : {
--         (idx) +_rhs_5239 :
--             0 : dependencies = [ gtid_5232 0 par ]
--             1 : dependencies = [ i_5236 1 seq ]
--     }
-- }
