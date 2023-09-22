def main [n][m] (xss: [n][m]i64) : [m]i64 =
    foldl (\acc xs -> map2 (\a x -> x+a) acc xs) (replicate m 0) xss

-- === Expected output of analysis: CONFIRMED
-- (segmap) defunc_0_map_res_5450 : {
--     (arr) acc_5428 : {
--         (idx) eta_p_5453 :
--             0 : dependencies = [ gtid_5451 1 par ]
--     }
--     (arr) b_5429 : {
--         (idx) eta_p_5454 :
--             0 : dependencies = [ gtid_5451 1 par ]
--     }
-- }
