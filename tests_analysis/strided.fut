def main [n][m] (k: i64) (xss: [n][m]i64) : [n]i64 =
  map (\xs ->
    #[unsafe] 
    loop s=0 for i < m / k
    do
      s + xs[i * k] -- inv?
  ) xss

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5216 : {
--     (arr) xss_5148 : {
--         (idx) +_rhs_5224 :
--             0 : dependencies = {gtid_5217 0 par}
--             1 : dependencies = {k_5147 0 seq, i_5221 1 seq}
--     }
-- }
