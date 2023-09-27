def main [n][m] (is: [m]i64) (xss: [n][m]i64) : [n]i64 =
  map (\xs ->
    #[unsafe]
    loop s=0 for _i < m
    do
      let k = 5
      in s + xs[ is[ k ] ]
  ) xss

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5244 : {
--     (arr) xss_5163 : {
--         (idx) x_5249 :
--             0 : dependencies = {gtid_5245 0 par}
--             1 : dependencies = {}
--     }
--     (arr) +_rhs_dev_5254 : {
--         (idx) +_rhs_5255 :
--             0 : dependencies = {}
--     }
-- }
