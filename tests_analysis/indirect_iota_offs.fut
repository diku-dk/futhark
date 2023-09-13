def main [n] (xs: [n]i64) : [n]i64 =
  let is = iota n
  in map (\i -> #[unsafe] xs[is[i+2]] ) (iota n)

-- === Expected output of analysis: CONFIRMED
-- entry_main
--   defunc_0_map_res_5261 => [
--     xs_5199
--       [ index_primexp_5265 | ν par ]
--   ]
