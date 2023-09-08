def main [n] (xs: [n]i64) : [n]i64 =
  let is = replicate n 0
  in map (\i -> #[unsafe] xs[is[i]] ) (iota n)

-- === Expected output of analysis:
-- entry_main
--   defunc_0_map_res_5279 => [
--     defunc_0_map_res_5282
--       [ σ defunc_0_map_res_5282 | ν par ]  [ τ 0i64 | ψ ]
--   ]
--   defunc_0_map_res_5282 => []