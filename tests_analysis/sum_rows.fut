-- Sum rows in a matrix
def main [n] (xss: [n][n]i32) : [n]i32 =
--   #[incremental_flattening(no_intra)]
--   #[incremental_flattening(only_inner)]
    -- map i32.sum xss
    map (\xs ->
        loop acc=0 for i < n do
            acc + xs[i]
    ) xss

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5183 : {
--     (arr) (xss_coalesced_5192, []) : {
--         (idx) +_rhs_5190 :
--             0 : dependencies = {gtid_5184 0 par}
--             1 : dependencies = {i_5188 1 seq}
--     }
-- }
