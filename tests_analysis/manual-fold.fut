def main [n][m] (xss: [n][m]i64) : [n]i64 =
  map (\xs ->
    #[unsafe] 
    loop s=0 for i < m
    do
      s + xs[i]
  ) xss


-- === Expected output of analysis:
-- entry_main
--   xsss_transformed_5310 [[σ xss_5128 | ν | par], [σ xss_5128 | ν | seq]]
-- WARNING: This output might be completely wrong!!!!!!!