def main [n] (xs: [n]i64) : [n]i64 =
  let is = iota n
  let is_2 = reverse is
  in map (\i -> #[unsafe] xs[is[is_2[i]]]) (iota n)

-- === Expected output of analysis:
-- entry_main
--   w_minus_1_5286 []
--   segmap_usable_groups_5320 []
--   slice_5325 []
--   lifted_lambda_res_5326 [[σ slice_5325 | ν | par]]