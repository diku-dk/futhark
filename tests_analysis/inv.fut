-- Make sure 'i*0' reduces to ψ
def main [n] (xs: [n]i64) : [n]i64 =
  map (\i ->  #[unsafe] xs[i*0] ) (iota n)

-- === Expected output of analysis:
-- entry_main
--   defunc_0_map_res_5193 => [
--     defunc_0_map_res_5198
--       [ 0i64 :+ n_5161 * 1i64 | ψ ] [ 0i64 | ψ ]
--   ]
--   lifted_lambda_res_dev_5197 => [
--     xs_5162
--       [ 0i64 | ψ ]
--   ]
