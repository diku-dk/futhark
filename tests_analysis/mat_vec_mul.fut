-- Matrix-vector multiplication
def main [n] (A: [n][n]i32) (v: [n]i32) : [n]i32 =
    let vs = replicate n v
    in map2 (\A_row vs_col ->
        foldl (+) 0 (map2 (*) A_row vs_col)
    ) A vs

-- === Expected output of analysis:
-- entry_main
--   defunc_0_map_res_r_5726 => [
--     A_5513
--       [ gtid_5727 | ν par ] [ gtid_5728 | ν par ]
--     v_5514
--       [ gtid_5728 | ν par ]
--   ]
--   defunc_0_map_res_5737 => [
--     defunc_0_map_res_r_5726
--       [ gtid_5738 | ν par ] [ 0i64 :+ n_5512 * 1i64 | ψ ]
--   ]