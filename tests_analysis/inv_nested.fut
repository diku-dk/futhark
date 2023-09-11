def main [n][m] (xss: [n][m]i64) : [n][m]i64 =
  let k = foldl (+) 0 (map (\i -> i*0) (iota n))
  let l = map (\i -> i*k) (iota n)
  in map (\i -> #[unsafe] xss[i]) l

-- === Expected output of analysis:
-- entry_main
--   lifted_lambda_res_5410 => [
--     xss_5335
--       [ τ 0i64 | ψ ] [ σ xss_5335 | ν par ]
--   ]
