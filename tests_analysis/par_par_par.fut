def main [l][n][m] (xsss: [l][n][m]i64) : [l][n][m]i64 =
  map (\xss -> map (\xs -> map (+2) xs) xss) xsss

-- === Expected output of analysis:
-- entry_main
--   y_5481 []
--   nest_size_5482 []
--   segmap_usable_groups_5484 []
--   eta_p_5490 [[σ gtid_5486 | ν | par], [σ gtid_5487 | ν | par], [σ gtid_5488 | ν | par]]
--   lifted_lambda_res_5491 []