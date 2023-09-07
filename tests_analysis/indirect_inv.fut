def main [n][m] (is: [m]i64) (xss: [n][m]i64) : [n]i64 =
  let k = 5
  in map (\i -> #[unsafe] xss[is[k],i] ) (iota n)


-- === Expected output of analysis:
-- entry_main
--   defunc_0_map_res_5299 []
--   tmp_5302 [[τ 5i64 | ψ ]]
--   xss_prefix_5305 [[σ tmp_5302 | ψ ], [σ xss_5242 | ν | par]]