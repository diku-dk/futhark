def main [n] (xs: [n]i64) : [n]i64 =
  let is = replicate n 0
  in map (\i -> #[unsafe] xs[is[i]] ) (iota n)

-- === Expected output of analysis: CONFIRMED
-- entry_main
--   defunc_0_map_res_5279 => [
--     defunc_0_map_res_5282
--       [ 0i64 :+ n_5240 * 1i64 | ψ ] [ 0i64 | ψ ]
--   ]
--   lifted_lambda_res_dev_5281 => [
--     xs_5241
--       [ 0i64 | ψ ]
--   ]
