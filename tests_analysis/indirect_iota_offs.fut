def main [n] (xs: [n]i64) : [n]i64 =
  let is = iota n
  in map (\i -> #[unsafe] xs[is[i+2]] ) (iota n)

-- === Expected output of analysis:
-- entry_main
--   segmap_usable_groups_5260

--   index_primexp_5265

--   lifted_lambda_res_5266
--     xs_5199 [[σ index_primexp_5265 | ν | par]]