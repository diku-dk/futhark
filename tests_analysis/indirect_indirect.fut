def main [n] (xs: [n]i64) : [n]i64 =
  let is = iota n
  let is_2 = reverse is
  in map (\i -> #[unsafe] xs[is[is_2[i]]]) (iota n)

-- === Expected output of analysis:
-- entry_main
--   defunc_0_map_res_5321 => [
--     xs_5242
--       [ σ slice_5325 | ν par ]
--   ]