def main [n][m] (xss: [n][m]i64) : [m]i64 =
    foldl (\acc xs -> map2 (\a x -> x+a) acc xs) (replicate m 0) xss

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5451 : {
--     (arr) xss_5346 : {
--         (idx) eta_p_5455 :
--             0 : dependencies = {i_5426 0 seq}
--             1 : dependencies = {gtid_5452 1 par}
--     }
-- }
