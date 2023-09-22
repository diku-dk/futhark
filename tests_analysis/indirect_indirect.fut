def main [n][m] (xs: [n]i64) (is_0: [m]i64) (is_1: [m]i64) : [n]i64 =
  map (\i -> #[unsafe] xs[is_0[is_1[i]]]) (iota n)

-- === Expected output of analysis:
-- entry_main
--   defunc_0_map_res_5321 => [
--     xs_5242
--       [ slice_5325 | ν par ]
--   ]
