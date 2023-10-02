def main [l][n][m] (xsss: [l][n][m]i64) : [l][n]i64 =
  map (\xss ->
    map (\xs ->
      #[unsafe]
      loop res=xs[0] for i < n do
        res + xs[i]
    ) xss
  ) xsss

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5390 : {
--     (arr) xsss_5241 : {
--         (idx) tmp_5395 :
--             0 : dependencies = {gtid_5391 0 par}
--             1 : dependencies = {gtid_5392 1 par}
--             2 : dependencies = {}
--         (idx) +_rhs_5399 :
--             0 : dependencies = {gtid_5391 0 par}
--             1 : dependencies = {gtid_5392 1 par}
--             2 : dependencies = {i_5397 2 seq}
--     }
-- }
