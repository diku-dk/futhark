def main [l][n][m] (xsss: [l][n][m]i64) : [l]i64 =
  map (\xss ->
    loop res=0 for i < m do
      #[unsafe]
      res + xss[0][i]
  ) xsss

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5224 : {
--     (arr) xsss_5162 : {
--         (idx) +_rhs_5231 :
--             0 : dependencies = {gtid_5225 0 par}
--             1 : dependencies = {}
--             2 : dependencies = {i_5229 1 seq}
--     }
-- }
