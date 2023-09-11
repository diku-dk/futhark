def main [n][m] (is: [m]i64) (xss: [n][m]i64) : [n]i64 =
  let k = 5
  in map (\i -> #[unsafe] xss[is[k],i] ) (iota n)


-- === Expected output of analysis:
-- entry_main
--   tmp_5302 => [
--     is_5241
--       [ τ 5i64 | ψ ]
--   ]
--   xss_prefix_5305 => [
--     xss_5242
--       [ σ tmp_5302 | ψ ] [ σ xss_5242 | ν par ]
--   ]
